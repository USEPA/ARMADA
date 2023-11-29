console = d3.window(svg.node()).console;

d3.select(".active .chart-container")
    .append("div")
    .attr("class", "tooltip");

const tooltipDiv = d3.select(".active .tooltip");

r2d3.onRender(function(data, svg, width, height, options) {
	// Assign Marker Color Values
	marker_color = getMarkerColor(options.condition);
	marker_sig_color = getMarkerSigColor(options.condition);
	
	  let resource = options.resource
	  let units = options.units
	  let T1 = options.T1
	  let T2 = options.T2
	  let change = options.change
	  let state = options.state
	  let year = options.year
    let primary_data = data.filter(d => d["Subpopulation"] === options.primary_subpop);
    let primary_indicators = primary_data.map(d => d["Indicator"]);
    let indicator_to_index = Object.assign({}, ...primary_data.map((d,i) => ({[d.Indicator]: i})));

    let comp_data = data.filter(d => d["Subpopulation"] === options.comp_subpop && primary_indicators.includes(d["Indicator"]));
    
    
    /*********************************************
     * Condition Estimates Axes and Ranges
     *********************************************/

    let y_cond_est_padding = 0.5
    let y_cond_est = d3.scaleBand()
        .domain(d3.range(primary_data.length))
        .rangeRound([margin.top, options.height - margin.bottom])
        .paddingInner(y_cond_est_padding)
        .paddingOuter(y_cond_est_padding / 2.0);

    let outer_padding_top_offset = -3;
    let outer_padding_bottom_offset = -4;
    let outer_padding_bottom = ((y_cond_est.step() * (y_cond_est_padding / 2.0)) / 2.0) + outer_padding_bottom_offset;
    let outer_padding_top = ((y_cond_est.step() * (y_cond_est_padding / 2.0)) / 2.0) + outer_padding_top_offset;

    let x_cond_est = d3.scaleLinear()
        .domain([0, 120])
        .range([margin.left, cond_est_width + margin.left]);
        
    let y_axis_cond_est = g => g
        .attr("transform", `translate(${margin.left},0)`)
        .style("font-size", large_font_size)
        .call(d3.axisLeft(y_cond_est).tickFormat( i => primary_data[i]? primary_data[i].Indicator: primary_data[i].Indicator).tickSize(0))
        .call(g => g.select(".domain").remove())
        .call(g => g.selectAll(".tick line").clone()
            .attr("stroke-opacity", default_stroke_opacity)
            .attr("stroke", stroke_color)
            .attr("x2", dashboard_width - margin.right - margin.left)
            .attr("x1", -margin.left)
            .attr("transform", (d, i) => `translate(0,${y_cond_est.bandwidth()})`)
        )
        // Clone again but just draw the first line to make a top border
        .call(g => g.selectAll(".tick line").clone()
            .attr("stroke-opacity", (d, i) => i === 0 ? default_stroke_opacity : 0.0)
            .attr("stroke", stroke_color)
            .attr("x2", dashboard_width - margin.right - margin.left)
            .attr("x1", -margin.left)
            .attr("transform", (d, i) => `translate(0,${-y_cond_est.bandwidth()})`)
        );

    let x_axis_cond_est = g => g
        .attr("transform", `translate(0,${margin.top})`)
        .style("font-size", medium_font_size)
        .style("color", light_text_color)
        .call(d3.axisTop(x_cond_est)
            .ticks(cond_est_width / 80)
            .tickSize(5)
            .tickFormat(d => d + "%")
            .tickValues([0, 20, 40, 60, 80, 100])
        )
        .call(g => g.select(".domain").remove())
        .call(g =>
            g
            .selectAll(".tick line")
            .clone()
            .attr("stroke-opacity", default_stroke_opacity)
            .attr("stroke", stroke_color)
            .attr("y2", options.height - margin.top - margin.bottom - outer_padding_bottom)
        )
        // Clone again to create left border and right border
        .call(g =>
            g
            .selectAll(".tick line")
            .clone()
            .attr("stroke-opacity", (d, i) => [0, 1, 2, 3].includes(i) ? .9 : 0.0)
            .attr("stroke", stroke_color)
            .attr("y2", options.height - margin.top - margin.bottom - outer_padding_bottom)
            .attr("y1", outer_padding_top)
            .attr("transform", function(d, i) {
            	 if (i === 0) {
            	 	return `translate(${-margin.left},0)`;
            	 }
            	 else if (i === 1) {
            	 	return `translate(${dashboard_width-margin.left-margin.right-1},0)`;// -1px so that there are no gaps
            	 }
            	 else if (i === 2) {
            	 	return `translate(${cond_est_width - change_width/2},0)`;
            	 }
            	 else if (i === 3) {
            	 	return `translate(${cond_est_width - change_width*1.5},0)`;
            	 }
            })
        );

    let format = x_cond_est.tickFormat(20);

    /*********************************************
     * Change Axes and Ranges
     *********************************************/

    let x_change_padding = 0.1;
    let x_change_left_padding = 5;
    let x_change_right_padding = 5;
    let x_change = d3.scaleBand()
        .domain([T1, T2])
        .range([margin.left + cond_est_width + x_change_left_padding, dashboard_width - long_term_change_width - margin.right - x_change_right_padding])
        .padding(x_change_padding);

    let change_chart_height_padding = 10;
    let change_chart_height = y_cond_est.bandwidth() + change_chart_height_padding;

    let y_change = d3.scaleLinear()
        .domain([0, 100])
        .range([0, change_chart_height]);

    let y_axis_change = (g, i) => g
        .attr("transform", `translate(${margin.left+cond_est_width+x_change_left_padding},${y_cond_est(i) - (change_chart_height_padding/2.0)})`)
        .call(d3.axisLeft(y_change)
            .ticks(0)
            .tickSize(0)
            .tickFormat("")
            .tickValues([0, 50, 100])
        )
        .call(g => g.select(".domain").remove())
        .call(g => g.selectAll(".tick line").clone()
            .attr("stroke-opacity", default_stroke_opacity)
            .attr("stroke", stroke_color)
            .attr("x2", (x_change.bandwidth() * x_change.domain().length) + (x_change.step() * x_change_padding * (x_change.domain().length + 1)))
            .style("stroke-dasharray", (d, i) => i % 2 === 0 ? "" : "2, 2")
        );

    let x_axis_change = g => g
        .attr("transform", `translate(0,${margin.top})`)
        .style("font-size", small_font_size)
        .style("color", light_text_color)
        .call(d3.axisTop(x_change)
            .ticks(null)
            .tickSize(5)
        )
        .call(g => g.selectAll(".domain").remove());

    /*********************************************
     * Long Term Change Axes and Ranges
     *********************************************/

    let x_long_term_change = d3.scaleLinear()
        .domain([-100, 100])
        .rangeRound([margin.left + cond_est_width + change_width, dashboard_width - margin.right]);

    let x_axis_long_term_change = g => g
        .attr("transform", `translate(0,${margin.top})`)
        .style("font-size", medium_font_size)
        .style("color", light_text_color)
        .call(d3.axisTop(x_long_term_change)
            .ticks(null)
            .tickSize(5)
            .tickFormat(d => d + "%")
            .tickValues([-80, -60, -40, -20, 0, 20, 40, 60, 80])
        )
        .call(g => g.selectAll(".domain").remove())
        .call(g =>
            g
            .selectAll(".tick line")
            .clone()
            .attr("stroke-opacity", default_stroke_opacity)
            .attr("stroke", stroke_color)
            .attr("y2", options.height - margin.top - margin.bottom - outer_padding_bottom)
            .attr("stroke-opacity", (d, i) => i === 4 ? heavy_stroke_opacity : default_stroke_opacity)
        );

    // Remove all prior elements before re-drawing
    svg.selectAll("g").remove();

    // Set Height/Width/Viewbox
    svg.attr("preserveAspectRatio", "xMinYMin meet")
        .attr("viewBox", `0 0 ${dashboard_width} ${options.height}`)
        .attr("height", null)
        .attr("width", null);

    /*********************************************
     * Create Dashboard Axis Bar and Footer
     *********************************************/

    createDashAxisBar(svg, dashboard_width, x_cond_est, x_long_term_change, "Indicator", change, units);
    createTitle("all", options);
    createFooter(svg, options);
//    createWatermark(svg);

    /*********************************************
     * Draw Condition Estimates Chart
     *********************************************/

    svg.append("g")
        .call(x_axis_cond_est);

    svg.append("g")
        .call(y_axis_cond_est);

    cond_est_g = svg.append("g").attr("fill", marker_color);

    cond_est_g.selectAll(".cond-est-ci-bar-comp-subpop")
        .data(comp_data)
        .enter()
        .append("line")
        .attr("x1", (d) => x_cond_est(+d["T1.LCB"]))
        .attr("x2", (d) => x_cond_est(+d["T1.UCB"]))
        .attr("y1", (d, i) => y_cond_est(indicator_to_index[d["Indicator"]]) + y_cond_est.bandwidth()/2.0)
        .attr("y2", (d, i) => y_cond_est(indicator_to_index[d["Indicator"]]) + y_cond_est.bandwidth()/2.0)
        .attr("stroke", "grey")
        .attr("stroke-opacity", default_stroke_opacity)
        .style("opacity", default_stroke_opacity)
        .attr("stroke-width", 25)
        .call(tooltip, tooltipDiv, "cond_est comp");

    cond_est_g
        .selectAll("rect")
        .data(primary_data)
        .join("rect")
        .attr("x", x_cond_est(0))
        .attr("y", (d, i) => y_cond_est(i))
        .attr("width", d => x_cond_est(d["T1.P.Estimate"]) - x_cond_est(0))
        .attr("height", y_cond_est.bandwidth())
        .style("opacity", default_stroke_opacity)
        .call(tooltip, tooltipDiv, "cond_est");

    cond_est_g.selectAll(".cond-est-ci-bar-primary-subpop")
        .data(primary_data)
        .enter()
        .append("line")
        .attr("x1", (d) => x_cond_est(+d["T1.LCB"]))
        .attr("x2", (d) => x_cond_est(+d["T1.UCB"]))
        .attr("y1", (d, i) => y_cond_est(i) + y_cond_est.bandwidth()/2.0)
        .attr("y2", (d, i) => y_cond_est(i) + y_cond_est.bandwidth()/2.0)
        .attr("stroke", marker_color)
        .attr("stroke-width", ci_bar_height)
        .call(tooltip, tooltipDiv, "cond_est");

    let letter_width = 4.5;
    let label_x_offset = 11;
    svg.append("g")
        .attr("text-anchor", "end")
        .attr("font-size", medium_font_size)
        .selectAll("text")
        .data(primary_data)
        .join("text")
        .attr("fill", d => getLabel(options.label_format, d) == "N/A"? light_text_color: dark_text_color)
        .attr("x", d => Math.max(x_cond_est(+d["T1.UCB"]), x_cond_est(+d["T1.P.Estimate"])))
        .attr("y", (d, i) => y_cond_est(i) + y_cond_est.bandwidth() / 2)
        .attr("dy", "0.35em")
        .attr("dx", d => getLabel(options.label_format, d).length * letter_width + label_x_offset)
        .text(function(d) {
        	return getLabel(options.label_format, d);
		})
        .call(text => text.filter(d => x_cond_est(+d["T1.UCB"]) - x_cond_est(0) < 1) // short bars
            .attr("dx", +4)
            .attr("fill", d => getLabel(options.label_format, d) == "N/A"? light_text_color: dark_text_color)
            .attr("text-anchor", "start"));

    /*********************************************
     * Draw Change Charts
     *********************************************/

    let change_g = svg.append("g").attr("fill", marker_color);

    change_g.selectAll(".bar-t1")
        .data(primary_data)
        .enter()
        .append("rect")
        .attr("x", x_change(T1))
        .attr("y", (d, i) => y_cond_est(i) - y_change(d["Early.P.Estimate"]) + y_cond_est.bandwidth() + change_chart_height_padding / 2.0)
        .attr("height", d => y_change(d["Early.P.Estimate"]))
        .attr("width", x_change.bandwidth());

    change_g.selectAll(".bar-t2")
        .data(primary_data)
        .enter()
        .append("rect")
        .attr("x", x_change(T2))
        .attr("y", (d, i) => y_cond_est(i) - y_change(d["changeT1.P.Estimate"]) + y_cond_est.bandwidth() + change_chart_height_padding / 2.0)
        .attr("height", d => y_change(d["changeT1.P.Estimate"]))
        .attr("width", x_change.bandwidth());

    svg.append("g")
        .call(x_axis_change);

    for (let i = 0; i < primary_data.length; i++) {
        change_g.append("g")
            .attr("class", "change-grid")
            .call(y_axis_change, i);
    }

    change_g.selectAll(".change-tooltip-cover")
        .data(primary_data)
        .enter()
        .append("rect")
        .attr("x", x_change(T1) - x_change_padding * x_change.step())
        .attr("y", (d,i) => y_cond_est(i) - change_chart_height_padding / 2.0)
        .attr("height", change_chart_height)
        .attr("width", (x_change.bandwidth() * x_change.domain().length) + (x_change.step() * x_change_padding * (x_change.domain().length + 1)))
        .attr("fill", "red")
        .attr("fill-opacity", 0.0)
        .call(tooltip, tooltipDiv, "change");

    change_g.selectAll(".na-messages")
        .data(primary_data)
        .join("text")
        .attr("x", cond_est_width + margin.left + (change_width/2.0))
        .attr("y", (d, i) => y_cond_est(i) + y_cond_est.bandwidth() / 2)
        .attr("dy", "0.35em")
        .attr("text-anchor", "middle")
        .attr("font-size", medium_font_size)
        .attr("fill", light_text_color)
        .text(d => d["T1T2_DIFF.P"] === null? "N/A" : "");

    /*********************************************
     * Draw Long Term Change Chart
     *********************************************/

    svg.append("g")
        .call(x_axis_long_term_change);

    let long_term_change_g = svg.append("g").attr("fill", marker_color);

    long_term_change_g.selectAll("path")
        .data(primary_data)
        .enter()
        .append("path")
        .attr("d", d3.symbol().type(d3.symbolDiamond))
        .style("fill", (d) => d["Sig95.Flag.Diff.T2vT3.Condition"] === "Y"? marker_sig_color:marker_color)
        .attr("transform", (d, i) => `translate(${x_long_term_change(+d["T1T2_DIFF.P"])}, ${y_cond_est(i) + (y_cond_est.bandwidth()/2.0)})`)
        .attr("fill-opacity", d => d["T1T2_DIFF.P"] === null? 0.0: 1.0);

    long_term_change_g.selectAll(".long-term-change-ci-bar")
        .data(primary_data)
        .enter()
        .append("line")
        .attr("x1", (d) => x_long_term_change(+d["CHANGE_LCB"]))
        .attr("x2", (d) => x_long_term_change(+d["CHANGE_UCB"]))
        .attr("y1", (d, i) => y_cond_est(i) + (y_cond_est.bandwidth() / 2.0))
        .attr("y2", (d, i) => y_cond_est(i) + (y_cond_est.bandwidth() / 2.0))
        .style("stroke", (d) => d["Sig95.Flag.Diff.T2vT3.Condition"] === "Y"? marker_sig_color:marker_color)
        .attr("stroke-width", ci_bar_height);

    long_term_change_g.selectAll(".long-term-change-tooltip-bar")
        .data(primary_data)
        .enter()
        .append("line")
        .attr("x1", (d) => +d["CHANGE_LCB"] === 0? x_long_term_change(-10) : x_long_term_change(+d["CHANGE_LCB"]))
        .attr("x2", (d) => +d["CHANGE_UCB"] === 0? x_long_term_change(10) : x_long_term_change(+d["CHANGE_UCB"]))
        .attr("y1", (d, i) => y_cond_est(i) + (y_cond_est.bandwidth() / 2.0))
        .attr("y2", (d, i) => y_cond_est(i) + (y_cond_est.bandwidth() / 2.0))
        .style("stroke", "red")
        .style("stroke-opacity", 0.0)
        .attr("stroke-width", change_chart_height)
        .call(tooltip, tooltipDiv, "long_term_change");


    long_term_change_g.selectAll(".na-messages")
        .data(primary_data)
        .join("text")
        .attr("x", x_long_term_change(0))
        .attr("y", (d, i) => y_cond_est(i) + y_cond_est.bandwidth() / 2)
        .attr("dy", "0.35em")
        .attr("text-anchor", "middle")
        .attr("font-size", medium_font_size)
        .attr("fill", light_text_color)
        .text(d => d["T1T2_DIFF.P"] === null? "Only One Time Period Available" : "");


    /*********************************************
     * Style Ticks and Tick Labels
     *********************************************/
    svg.selectAll(".tick line").attr("stroke", stroke_color);
    svg.selectAll("g").attr("font-family", dashboard_font_family);

    let controls_container_offset = 60.0;
    console.log("width all ", width);
    d3.select("#custom .controls-container").style("margin-top", width < 740? "0px": ((width/dashboard_width) * controls_container_offset) + "px");

});
