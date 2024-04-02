console = d3.window(svg.node()).console;

// Chart Setup Declarations
let margin = ({ top: options.margin_top, right: 0, bottom: options.margin_bottom, left: 225 });
let dashboard_width = 900;

let stroke_color = "#ddd";
let change_color = "#5c1a9e";

let default_stroke_opacity = 0.5;
let heavy_stroke_opacity = 1;

let text_label_color = "rgba(117, 117, 117, 1)";
let light_text_color = "#737373";
let dark_text_color = "black"; //"#333";

let dashboard_font_family = "'Roboto', sans-serif";
let large_font_size = "1.2rem";
let medium_font_size = "1.1rem";
let small_font_size = ".8rem";
let esmall_font_size = ".6rem";
// Condition Category Colors
let good_color = "rgba(107, 163, 214, 1)";
let good_sig_color = "rgba(62, 92, 210, 1)";
let fair_color = "rgba(255, 193, 86, 1)";
let fair_sig_color = "rgba(250, 159, 26, 1)";
let poor_color = "#f55b5b";
let poor_sig_color = "rgba(233, 24, 24, 1)";
let na_color = "rgba(210, 153, 125, 1)";
let na_sig_color = "rgba(153, 101, 75, 1)";

// Specify Defaults
let marker_color = good_color;
let marker_sig_color = good_sig_color;

// Dashboard Section Widths
let cond_est_width = (dashboard_width - margin.left - margin.right) * .45;
let change_width = (dashboard_width - margin.left - margin.right) * .05;
let long_term_change_width = (dashboard_width - margin.left - margin.right) * .5;
let dash_axis_bar_height = 30;

let ci_bar_height = 2;

let control_bar_height_str = d3.select(".controls-container").style("height");
let control_bar_height = parseFloat(control_bar_height_str.substring(0, control_bar_height_str.length - 2));

let significance_footnote = "<span class='footer-semi-bold'>Represents that the Indicator is identified as a <a href='https://www.epa.gov/sites/default/files/2014-10/documents/handbook-chapter2.pdf' target='_blank'>Designated Use</a> or <a href='https://www.epa.gov/sites/default/files/2018-10/documents/biological-assessments-criteria-crucial-components.pdf' target='_blank'>Ecological Assessment</a> by the State/Territory/Tribe in the ATTAINS database.</span>";


