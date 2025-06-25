// Global variables
let map;
let citiesData = [];
let countiesData = {};
let countiesLayer;
let currentYear = 1810;
let currentDemographic = "totalDens";
let animationInterval;
let histogram;

// Color palette similar to R's YlGnBu
const colorPalette = [
  "#ffffd9",
  "#edf8b1",
  "#c7e9b4",
  "#7fcdbb",
  "#41b6c4",
  "#1d91c0",
  "#225ea8",
  "#253495",
  "#081d58",
];

// Initialize the application
document.addEventListener("DOMContentLoaded", function () {
  initializeMap();
  loadData();
  setupEventListeners();
  loadAttributions();
  checkFirstVisit();
});

function initializeMap() {
  // Initialize Leaflet map
  map = L.map("cities_map", {
    center: [43.25, -94.3],
    zoom: 6,
    minZoom: 4,
    maxZoom: 8,
  });

  // Add CartoDB Positron tiles
  L.tileLayer(
    "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png",
    {
      attribution:
        '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors &copy; <a href="https://carto.com/attributions">CARTO</a>',
      subdomains: "abcd",
      maxZoom: 19,
    },
  ).addTo(map);
}

async function loadData() {
  try {
    // Load cities data
    citiesData = await d3.csv("data/midwest-cities.csv", (d) => ({
      id: +d.id,
      state: d.st,
      city: d.city,
      cityState: d.cityst,
      county: d.county_name,
      year: +d.year,
      population: +d.population,
      lat: +d.lat,
      lng: +d.lng,
    }));

    console.log("Cities data loaded:", citiesData.length, "records");

    // Load county demographic data for all years
    await loadCountyData();

    // Initialize visualization with default year
    updateVisualization();
    updateHistogram();
  } catch (error) {
    console.error("Error loading data:", error);
  }
}

async function loadCountyData() {
  const years = [];
  for (let year = 1810; year <= 2010; year += 10) {
    years.push(year);
  }

  for (const year of years) {
    try {
      const response = await fetch(`shp/merge_${year}.geojson`);
      const data = await response.json();
      countiesData[year] = data;
      console.log(
        `Loaded county data for ${year}:`,
        data.features.length,
        "counties",
      );
    } catch (error) {
      console.warn(`Could not load county data for ${year}:`, error);
    }
  }
}

function setupEventListeners() {
  const yearSlider = document.getElementById("year-slider");
  const yearDisplay = document.getElementById("year-display");
  const populationSelect = document.getElementById("population-select");
  const playButton = document.getElementById("play-button");
  const resetButton = document.getElementById("reset-button");
  const infoButton = document.getElementById("info-button");
  const modal = document.getElementById("info-modal");
  const closeButton = document.getElementById("close-modal");
  const modalCloseButton = document.getElementById("modal-close-button");

  // Year slider
  yearSlider.addEventListener("input", function () {
    currentYear = +this.value;
    yearDisplay.textContent = currentYear;
    updateVisualization();
    updateHistogram();
  });

  // Demographics selection
  populationSelect.addEventListener("change", function () {
    currentDemographic = this.value;
    updateVisualization();
    updateHistogram();
  });

  // Play animation
  playButton.addEventListener("click", function () {
    if (animationInterval) {
      clearInterval(animationInterval);
      animationInterval = null;
      this.textContent = "▶";
    } else {
      startAnimation();
      this.textContent = "⏸";
    }
  });

  // Reset to beginning
  resetButton.addEventListener("click", function () {
    if (animationInterval) {
      clearInterval(animationInterval);
      animationInterval = null;
      playButton.textContent = "▶";
    }
    currentYear = 1810;
    yearSlider.value = currentYear;
    yearDisplay.textContent = currentYear;
    updateVisualization();
    updateHistogram();
  });

  // Modal event listeners
  infoButton.addEventListener("click", showModal);
  closeButton.addEventListener("click", hideModal);
  modalCloseButton.addEventListener("click", hideModal);
  
  // Close modal when clicking outside of it
  modal.addEventListener("click", function(event) {
    if (event.target === modal) {
      hideModal();
    }
  });
}

function startAnimation() {
  animationInterval = setInterval(function () {
    if (currentYear >= 2010) {
      clearInterval(animationInterval);
      animationInterval = null;
      document.getElementById("play-button").textContent = "▶";
      return;
    }

    currentYear += 10;
    document.getElementById("year-slider").value = currentYear;
    document.getElementById("year-display").textContent = currentYear;
    updateVisualization();
    updateHistogram();
  }, 1400);
}

function updateVisualization() {
  // Clear existing markers and counties layer
  map.eachLayer(function (layer) {
    if (layer instanceof L.CircleMarker) {
      map.removeLayer(layer);
    }
  });

  if (countiesLayer) {
    map.removeLayer(countiesLayer);
  }

  // If demographic is "None", just show cities
  if (currentDemographic === "None") {
    showCitiesOnly();
    return;
  }

  // Show counties with demographic coloring
  if (countiesData[currentYear]) {
    showCountiesWithDemographics();
  }

  // Always show cities on top
  showCitiesOnly();
}

function showCitiesOnly() {
  // Filter cities by current year
  const currentCities = citiesData.filter((d) => d.year === currentYear);

  // Calculate radius scale
  const maxPop = Math.sqrt(Math.max(...citiesData.map((d) => d.population)));

  // Add city markers
  currentCities.forEach((city) => {
    const radius = Math.max(
      1,
      Math.min(55, (Math.sqrt(city.population) / maxPop) * 55),
    );

    const marker = L.circleMarker([city.lat, city.lng], {
      radius: radius,
      fillColor: "#B53D35",
      fillOpacity: 0.7,
      color: "#fff",
      weight: 0.5,
    });

    // Add popup
    const popupContent = `
            <h5>${city.city}</h5>
            <b>County: </b>${city.county}<br>
            <b>Population in ${city.year}: </b>${city.population.toLocaleString()}
        `;
    marker.bindPopup(popupContent);

    marker.addTo(map);
  });
}

function showCountiesWithDemographics() {
  const currentCounties = countiesData[currentYear];
  if (!currentCounties) return;

  // Get values for the current demographic
  const values = currentCounties.features
    .map((feature) => feature.properties[currentDemographic])
    .filter((val) => val != null && !isNaN(val));

  if (values.length === 0) return;

  // Create color scale
  const colorScale = getColorScale(values);

  // Create counties layer
  countiesLayer = L.geoJSON(currentCounties, {
    style: function (feature) {
      const value = feature.properties[currentDemographic];
      return {
        fillColor: colorScale(value),
        weight: 1,
        opacity: 0.8,
        color: "#ffffff",
        dashArray: "",
        fillOpacity: 0.6,
      };
    },
    onEachFeature: function (feature, layer) {
      const props = feature.properties;
      const value = props[currentDemographic];

      // Create popup content
      let popupContent = `
                <h5>${props.NHGISNAM} County, ${props.STATENAM}</h5>
                <b>Year: </b>${currentYear}<br>
            `;

      // Add demographic-specific information
      if (currentDemographic === "totalPop") {
        popupContent += `<b>Total Population: </b>${value ? value.toLocaleString() : "N/A"}`;
      } else if (currentDemographic === "totalDens") {
        popupContent += `<b>Population Density: </b>${value ? value.toFixed(2) + " per sq km" : "N/A"}`;
      } else if (currentDemographic === "totalAfAm") {
        popupContent += `<b>Black Population: </b>${value ? value.toLocaleString() : "N/A"}`;
      } else if (currentDemographic === "slavePop") {
        popupContent += `<b>Enslaved Population: </b>${value ? value.toLocaleString() : "N/A"}`;
      } else if (currentDemographic === "freeAfAm") {
        popupContent += `<b>Free Black Population: </b>${value ? value.toLocaleString() : "N/A"}`;
      } else if (currentDemographic === "totalAsian") {
        popupContent += `<b>Asian Population: </b>${value ? value.toLocaleString() : "N/A"}`;
      } else if (currentDemographic === "totalHispanic") {
        popupContent += `<b>Latino Population: </b>${value ? value.toLocaleString() : "N/A"}`;
      } else if (currentDemographic === "totalIndian") {
        popupContent += `<b>Native Population: </b>${value ? value.toLocaleString() : "N/A"}`;
      }

      layer.bindPopup(popupContent);
    },
  }).addTo(map);
}

function updateHistogram() {
  // Always show city population histogram regardless of demographic selection
  showCityHistogram();
}

function showCityHistogram() {
  const currentCities = citiesData.filter((d) => d.year === currentYear);
  const populations = currentCities.map((d) => d.population);

  if (populations.length === 0) return;

  // Create histogram bins (log scale)
  const logPops = populations.map((p) => Math.log10(p));
  const minLog = Math.min(...logPops);
  const maxLog = Math.max(...logPops);
  const binCount = 15;
  const binWidth = (maxLog - minLog) / binCount;

  const bins = Array(binCount).fill(0);
  const binLabels = [];

  for (let i = 0; i < binCount; i++) {
    const binStart = minLog + i * binWidth;
    const binEnd = minLog + (i + 1) * binWidth;
    binLabels.push(Math.pow(10, binStart).toFixed(0));

    logPops.forEach((logPop) => {
      if (
        logPop >= binStart &&
        (i === binCount - 1 ? logPop <= binEnd : logPop < binEnd)
      ) {
        bins[i]++;
      }
    });
  }

  createHistogramChart(
    bins,
    binLabels,
    `Midwest cities in ${currentYear}`,
    "Population",
    "Number of cities",
  );
}

function showCountyHistogram() {
  const currentCounties = countiesData[currentYear];
  if (!currentCounties) return;

  // Get values for the current demographic
  const values = currentCounties.features
    .map((feature) => feature.properties[currentDemographic])
    .filter((val) => val != null && !isNaN(val) && val > 0);

  if (values.length === 0) return;

  // Create histogram bins
  const minVal = Math.min(...values);
  const maxVal = Math.max(...values);
  const binCount = 15;
  const binWidth = (maxVal - minVal) / binCount;

  const bins = Array(binCount).fill(0);
  const binLabels = [];

  for (let i = 0; i < binCount; i++) {
    const binStart = minVal + i * binWidth;
    const binEnd = minVal + (i + 1) * binWidth;
    binLabels.push(binStart.toFixed(0));

    values.forEach((val) => {
      if (
        val >= binStart &&
        (i === binCount - 1 ? val <= binEnd : val < binEnd)
      ) {
        bins[i]++;
      }
    });
  }

  // Get demographic name for title and axis
  const demographicNames = {
    totalPop: "Total Population",
    totalDens: "Population Density",
    totalAfAm: "Black Population",
    slavePop: "Enslaved Population",
    freeAfAm: "Free Black Population",
    totalAsian: "Asian Population",
    totalHispanic: "Latino Population",
    totalIndian: "Native Population",
  };

  const demographicName =
    demographicNames[currentDemographic] || currentDemographic;
  const title = `${demographicName} in ${currentYear}`;
  const xAxisLabel =
    currentDemographic === "totalDens"
      ? "Population Density (per sq km)"
      : demographicName;

  createHistogramChart(
    bins,
    binLabels,
    title,
    xAxisLabel,
    "Number of counties",
  );
}

function createHistogramChart(bins, binLabels, title, xAxisLabel, yAxisLabel) {
  const ctx = document.getElementById("cities-histogram").getContext("2d");

  if (histogram) {
    histogram.destroy();
  }

  histogram = new Chart(ctx, {
    type: "bar",
    data: {
      labels: binLabels,
      datasets: [
        {
          data: bins,
          backgroundColor: "#B53D35",
          borderColor: "#fff",
          borderWidth: 1,
        },
      ],
    },
    options: {
      responsive: true,
      maintainAspectRatio: false,
      plugins: {
        legend: { display: false },
        title: {
          display: true,
          text: title,
        },
      },
      scales: {
        x: {
          title: {
            display: true,
            text: xAxisLabel,
          },
          ticks: {
            maxRotation: 45,
            callback: function (value, index) {
              const val = parseInt(this.getLabelForValue(value));
              return val >= 1000000
                ? (val / 1000000).toFixed(1) + "M"
                : val >= 1000
                  ? (val / 1000).toFixed(0) + "K"
                  : val;
            },
          },
        },
        y: {
          title: {
            display: true,
            text: yAxisLabel,
          },
          beginAtZero: true,
        },
      },
    },
  });
}

async function loadAttributions() {
  try {
    const cestaResponse = await fetch("cesta_attr.html");
    const cestaAttr = await cestaResponse.text();
    document.getElementById("cesta-attribution").innerHTML = cestaAttr;

    const generalResponse = await fetch("attr.html");
    const generalAttr = await generalResponse.text();
    document.getElementById("general-attribution").innerHTML = generalAttr;
  } catch (error) {
    console.error("Error loading attributions:", error);
  }
}

// Utility functions for color scaling (similar to R's colorQuantile)
function getColorScale(values, colors = colorPalette) {
  if (!values || values.length === 0) return () => "#ffffff";

  const validValues = values.filter((v) => v != null && !isNaN(v));
  if (validValues.length === 0) return () => "#ffffff";

  const quantiles = [];
  for (let i = 0; i <= colors.length; i++) {
    quantiles.push(
      d3.quantile(
        validValues.sort((a, b) => a - b),
        i / colors.length,
      ),
    );
  }

  return function (value) {
    if (value == null || isNaN(value)) return "#ffffff";

    for (let i = 0; i < quantiles.length - 1; i++) {
      if (value <= quantiles[i + 1]) {
        return colors[i];
      }
    }
    return colors[colors.length - 1];
  };
}

// Format numbers with commas
function formatNumber(num) {
  return num.toLocaleString();
}

// Modal functions
function showModal() {
  const modal = document.getElementById("info-modal");
  modal.style.display = "block";
  document.body.style.overflow = "hidden"; // Prevent background scrolling
}

function hideModal() {
  const modal = document.getElementById("info-modal");
  modal.style.display = "none";
  document.body.style.overflow = ""; // Restore scrolling
}

// Check if this is the user's first visit
function checkFirstVisit() {
  const hasVisited = localStorage.getItem("midwest-map-visited");
  if (!hasVisited) {
    // Show modal after a brief delay to ensure everything is loaded
    setTimeout(showModal, 1000);
    localStorage.setItem("midwest-map-visited", "true");
  }
}

