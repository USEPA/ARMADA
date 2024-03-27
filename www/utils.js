// Define the function to wrap text
function wrap(text, width) {
  text.each(function () {
    const text = d3.select(this);
    const words = text.text().split(/\s+/).reverse();
    let word;
    let line = [];
    let lineNumber = 0;
    const lineHeight = 1.1; // Line height
   const y = text.attr("y");
    const dy = parseFloat(text.attr("dy"));
    let tspan = text.text(null).append("tspan").attr("x", 0).attr("y", y).attr("dy", dy + "em");

    while (word = words.pop()) {
      line.push(word);
      tspan.text(line.join(" "));
      if (tspan.node().getComputedTextLength() > width) {
        line.pop();
        tspan.text(line.join(" "));
        line = [word];
        tspan = text.append("tspan")
        .attr("x", 0)
        .attr("y", y)
        .attr('dx', '-.5em')
        .attr("dy", ++lineNumber * lineHeight + dy + "em").text(word);
      }
    }
    if (lineNumber > 0) {
        text.selectAll("tspan")
         .attr("y", -7);
        }
  });
}

function getFormattedLabel(d) {

	let formatData = d3.format(".0f");

	if (d !== 0)  {
		if (d > 99 && d < 100.0) {
			return ">99";
		}
		else if (d < 1 && d > 0.0) {
			return "<1";
		}
	}

	if (d === null) {
		return "NA";
	}

	return formatData(d);
}

function isRecordNA(view, d) {
    if (d['T1.P.Estimate'] === null)
        return true;
    else
        return false;
}

/* This needs to be CategoryConditionColor!*/
function getMarkerColor(condition) {
	if (condition==="Pass"||condition==="Good"||condition==="Supporting Use"||condition==="Meeting"||condition==="Fully Supporting"||condition==="Meets"||condition==="Supports"||condition==="Support"||condition==="Excellent"||condition==="Not Detected"||condition==="At Or Below Benchmark"||condition==="Low"||condition==="Attaining"||condition==="Very Good"||condition==="Good Condition"||condition==="Excellent Condition"||condition==="Least Disturbed"||condition==="Optimal")
		return good_color;
	else if (condition === "Fair"||condition === "Partially Supporting"||condition==="Satisfactory"||condition==="Moderate"||condition==="Potentially Not Supporting"||condition==="Fair Condition"||condition==="Intermediate")
		return fair_color;
	else if (condition === "Fail"||condition==="Poor"||condition==="Not Supporting Use"||condition==="Violating"||condition==="Not Supporting"||condition==="Violates"||condition==="Violates Natural"||condition==="Impaired"||condition==="Detected"||condition==="Above Benchmark"||condition==="High"||condition==="Poor Condition"||condition==="Most Disturbed"||condition==="Suboptimal")
		return poor_color;
	else
		return na_color;
}

/* This needs to be CategoryConditionColor!*/
function getMarkerSigColor(condition) {
	if (condition==="Pass"||condition==="Good"||condition==="Supporting Use"||condition==="Meeting"||condition==="Fully Supporting"||condition==="Meets"||condition==="Supports"||condition==="Support"||condition==="Excellent"||condition==="Not Detected"||condition==="At or Below Benchmark"||condition==="Low"||condition==="Attaining"||condition==="Very Good"||condition==="Good Condition"||condition==="Excellent Condition"||condition==="Least Disturbed"||condition==="Optimal")
		return good_color;
	else if (condition === "Fair"||condition === "Partially Supporting"||condition==="Satisfactory"||condition==="Moderate"||condition==="Potentially Not Supporting"||condition==="Fair Condition"||condition==="Intermediate")
		return fair_color;
	else if (condition === "Fail"||condition==="Poor"||condition==="Not Supporting Use"||condition==="Violating"||condition==="Not Supporting"||condition==="Violates"||condition==="Violates Natural"||condition==="Impaired"||condition==="Detected"||condition==="Above Benchmark"||condition==="High"||condition==="Poor Condition"||condition==="Most Disturbed"||condition==="Suboptimal")
		return poor_sig_color;
	else
		return na_sig_color;
}

function getLabel(label_format, d) {

	if (d["T1.P.Estimate"] === null || d["T1.P.Estimate"] === "NA") {
		return "N/A";
	}

	if (label_format === "Point Estimate")
		return getFormattedLabel(+d["T1.P.Estimate"]) + "%"
	else if (label_format === "Confidence Intervals")
		return getFormattedLabel(+d["T1.LCB"]) + " to " + getFormattedLabel(+d["T1.UCB"]) + "%";
	else
		return "";
}


function createDashAxisBar(svg, width, x_cond_est, x_long_term_change, cond_est_header, change, units) {
    
    
    let dash_axis_bar = svg.append("g")
    	.attr("fill", "black")
   		.attr("font-size", `15px`)
   		.attr("font-weight", "bold");
    
    let dash_axis_y = margin.top - (dash_axis_bar_height*2);
    let dash_axis_text_y = (dash_axis_bar_height - 10) + dash_axis_y;

    dash_axis_bar.append("rect")
    	.attr("x", 0)
    	.attr("y", dash_axis_y)
    	.attr("height", dash_axis_bar_height)
    	.attr("width", width)
    	.attr("fill", stroke_color);

    let dash_axis_bar_x_offset =3.5;
   	dash_axis_bar.append("text")
   		.attr("text-anchor", "middle")
   		.attr("x", margin.left * 0.75)
   		.attr("y", dash_axis_text_y)
   		.text(cond_est_header);

   	dash_axis_bar.append("text")
   		.attr("text-anchor", "middle")
   		.attr("x", x_cond_est(50))
   		.attr("y", dash_axis_text_y)
   		.text(`Percent of Resource (${units})`);

   	dash_axis_bar.append("text")
   		.attr("text-anchor", "middle")
   		.attr("x", cond_est_width + margin.left + (change_width/2.0))
   		.attr("y", dash_axis_text_y)
   		//.attr("fill", change_color)
   		.text("Change");
   	
   	dash_axis_bar.append("text")
   		.attr("text-anchor", "middle")
   		.attr("x", x_long_term_change(0))
   		.attr("y", dash_axis_text_y)
      //.attr("fill", change_color)
   		.text(change);
   		
}

/* function createWatermark(svg) {
  
   let watermark = svg.append("g").attr("class", "watermark");

   let watermark_width = 440;
   let watermark_height = 60;
   let watermark_offset = 95;

  watermark.append("text")
     .attr("text-anchor", "right")
     .attr("x", cond_est_width + change_width + watermark_offset)
     .attr("y", 0)
     .text("DRAFT - NOT FOR DISTRIBUTION - DATA ARE NOT FINAL");

   let wrap = d3.textwrap().bounds({height: watermark_height, width: watermark_width});
   svg.selectAll('.watermark text').call(wrap);

 }*/





function createFooter(svg, options) {

	let asterisk_offset = 5;
	let footnote_top_padding = 5;

	let footer = svg.append("g").attr("class", "footer");

   	footer.append("text")
   		.attr("text-anchor", "left")
   		.attr("x", 0)
   		.attr("y", options.height - margin.bottom + footnote_top_padding)
   		.text("*");

   	footer.append("text")
   		.attr("text-anchor", "left")
   		.attr("x", asterisk_offset)
   		.attr("y", options.height - margin.bottom + footnote_top_padding)
   		.text(significance_footnote);

    let footer_line_height = 16;
    footer.append("text")
      .attr("text-anchor", "left")
      .attr("x", 0)
      .attr("y", options.height - margin.bottom + footnote_top_padding + footer_line_height)
      .text(`<br>${options.survey_comment}<br><hr><span class='footer-semi-bold'>About the Dashboard</span>: This dashboard displays statistical survey results which provide an overall picture of water quality condition across a State/Territory/Tribe. From left to right, the graphs display the percentage of aquatic resources in different conditions for the most recent survey year available and a change comparison from the selected survey years. Please note that the years shown are the years survey data was reported and not necessarily the collection year. Explore different resource types, subpopulations, condition categories and survey years by using the dropdowns on the right. Hover over a result to see more information and an explanation of the results. For national survey data, please visit <a href='https://www.epa.gov/national-aquatic-resource-surveys'>EPA’s webpage for the National Aquatic Resource Surveys</a>.</span>`);

   	let wrap = d3.textwrap().bounds({height: margin.bottom - footnote_top_padding, width: dashboard_width - asterisk_offset})
   	svg.selectAll('.footer text').call(wrap);
}



function createTitle(view, options) { // primary_subpop, comparison_subpop, condition, indicator) {
  // options.primary_subpop, options.comp_subpop, options.condition, nul
	let header = svg.append("g").attr("class", "header");

	let title = `<h1>${options.state} | ${options.year} | Percent of ${options.resource} ${options.units} in ${options.condition} Category</h1>${options.primary_subpop} Estimates and ${options.change}`;
	if (view === "one") {
		title = `<h1>${options.state} | ${options.year} | Percent of ${options.resource} ${options.units} in Each Condition Category</h1><b><u>${options.use}</u></b> | ${options.primary_subpop} Estimates and ${options.change}`;
	}
//<span style="color:blue">
   	header.append("text")
   		.attr("text-anchor", "left")
   		.attr("x", 0)
   		.attr("y", 0)
   		.text(title);

   	let wrap = d3.textwrap().bounds({height: margin.top, width: dashboard_width})
   	svg.selectAll('.header text').call(wrap);

}

let tip_format = d3.format(".1f");
let formatNumber = d3.format(",");


