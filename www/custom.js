// Function to reset opacity for all traces
function resetOpacity(graphDiv) {
  var update = { opacity: Array(graphDiv.data.length).fill(1) };  // Set all traces to full opacity
  Plotly.restyle(graphDiv, update);
}

// Event listener for clicking on a trace
function addPlotlyClickListener(graphDiv) {
  graphDiv.on('plotly_click', function (data) {
    var update = { opacity: Array(graphDiv.data.length).fill(0.1) };  // Reduce opacity of all traces
    update.opacity[data.points[0].curveNumber] = 1;  // Keep full opacity for the clicked trace
    Plotly.restyle(graphDiv, update);  // Update the plot with new opacities
  });
}


// Event listener for double-clicking on the plot area
function addPlotlyDoubleClickListener(graphDiv) {
  graphDiv.on('plotly_doubleclick', function () {
    resetOpacity(graphDiv);  // Reset opacity of all traces
  });
}




//Function designed to handle highlighting detrended trace selected by user
plot = document.getElementById("tree_app-detrended_plot");


// Add event listener for input changes to detrended method to highlight the appropiate trace corresponding to the method
$(document).on('shiny:inputchanged', function (event) {
  if (event.name == 'tree_app-detrend_method') {
    console.log("Detrend method change");

    var update = { opacity: Array(plot.data.length).fill(0.1) };  // Reduce opacity of all traces

    trace_index = plot.data.findIndex(trace => trace.name === event.value)

    update.opacity[trace_index] = 1;  // Keep full opacity for the clicked trace
    Plotly.restyle(plot, update);  // Update the plot with new opacities
  };
});




$("#tree_app-raw_plot").on('plotly_click', function (data, points) {
  var series_name = (points.points[0].data.name);

  Shiny.setInputValue("tree_app-" + 'series_plot_selection', series_name,
    { priority: "event" })

});






// Detect when a different detrend method is selected in the series detrend table and update the reactive vals that are tracking these detrend methods for each series
function DetrendSeriesChange(ns) {

  $("#" + ns + "series_detrend_table").on('change', '.detrend-method-dropdown', function () {

    var row = $(this).closest('tr');
    var series = row.find('td').eq(0).text();
    var method = $(this).val();
    Shiny.setInputValue(ns + 'detrend_method_change', { series: series, method: method }, { priority: "event" });
  });
}
