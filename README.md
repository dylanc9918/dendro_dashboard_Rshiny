# Dendro Dashboard Shiny Application

This Shiny application is designed to provide a comprehensive dashboard for dendrochronology data analysis. The application allows users to visualize and analyze tree ring data, including detrending and other statistical methods.

## Capabilities

### Data Visualization

- **Interactive Map**: Visualize tree site locations on an interactive map using Leaflet. Click on markers to view detailed site information.
- **Data Tables**: Display raw and processed data in interactive tables using the DT package. Tables are collapsible for a cleaner interface.

### Data Analysis

- **Detrending Methods**: Apply various detrending methods to tree ring data, including:
  - Cubic Spline
  - Mod Negative Exponential
  - Mean
  - Autoregressive Model
  - Friedman Super Smoother
  - Modified Hugershoff
  - Age Dependent Spline
- **Signal Cleanup Options**: Options to pre-whiten chronology, strip EPS, and show sub-sample cutoff.

### Reporting

- **Detrend Report**: Generate a detailed report of the detrending methods applied, including EPS and S/N ratios.
- **Raw Data Plot**: Visualize raw tree ring data over time with interactive Plotly plots.
- **Detrended Data Plot**: Visualize detrended tree ring data with options to highlight specific series and reset the view.
