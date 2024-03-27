let getIndicatorTooltip = (d) => {
    return `${d['commentText']}`;
}

let getCondEstTooltip = (d) => {
    return `<strong>${d['Indicator']} | ${d['Condition']} | ${d['Subpopulation']}</strong><br>
            ${tip_format(d['T1.P.Estimate'])}% 
            (${tip_format(d['T1.LCB'])} to ${tip_format(d['T1.UCB'])}% @ ${tip_format(d['confidenceLevel'])}% confidence level) <hr style="margin: 10px;">
           <em> Explanation: In ${d['T1_Year']}, ${tip_format(d['T1.P.Estimate'])}% of ${d['Resource']} resources (${formatNumber(d['Condition_Size'])} of ${formatNumber(d['Survey_Size'])} ${d['Units']}) were in ${d['Condition']} category for ${d['Indicator']}. The confidence interval for this estimate is ${tip_format(d['T1.LCB'])}% to ${tip_format(d['T1.UCB'])}% (${formatNumber(d['T1.LCB_Size'])} to ${formatNumber(d['T1.UCB_Size'])} ${d['Units']}). The sample size supporting this estimate was ${d['siteNumber']} sites.</em>  <hr style="margin: 10px;">
${d['commentText']}`;
}
//${d['commentText']}
let getChangeTooltip = (d) => {
    return `<strong>${d['Indicator']} | ${d['Condition']} | ${d['Subpopulation']}</strong><br>
            <strong>${d['Early_Year']}:</strong> ${tip_format(d['Early.P.Estimate'])}% 
            (${tip_format(d['Early.LCB'])} to ${tip_format(d['Early.UCB'])}%)<br>
            <strong>${d['T1_Year']}:</strong> ${tip_format(d['T1.P.Estimate'])}% 
            (${tip_format(d['T1.LCB'])} to ${tip_format(d['T1.UCB'])}%) <hr style="margin: 10px;">
           <em> Explanation: In ${d['Early_Year']}, ${tip_format(d['Early.P.Estimate'])}% of ${d['Resource']} resources were in ${d['Condition']} condition for ${d['Indicator']}. In the most recent survey year (${d['T1_Year']}), the estimate has changed to  ${tip_format(d['T1.P.Estimate'])}%. </em><br>`;
}

let getLongTermChangeTooltip = (d) => {
    return `<strong>${d['Indicator']} | ${d['Condition']} | ${d['Subpopulation']}</strong><br>
            Change from ${d['Early_Year']} to ${d['T1_Year']}: ${tip_format(d['T1T2_DIFF.P'])}%  <hr style="margin: 10px;">
            <em> Explanation: The percentage of ${d['Resource']} resources in ${d['Condition']} condition for ${d['Indicator']} changed by ${tip_format(d['T1T2_DIFF.P'])}% from ${d['Early_Year']} to ${d['T1_Year']}. </em><br>`;
}


let tooltip = (selectionGroup, tooltipDiv, view) => {
    // padding between the tooltip and mouse cursor
    const MOUSE_POS_OFFSET = 8;

    selectionGroup.each(function() {
        d3.select(this)
            .on("mouseover", function() { handleMouseover(d3.select(this), view); })
            .on("mousemove", handleMousemove)
            .on("mouseleave", function() { handleMouseleave(d3.select(this), view); })
    });

    function handleMouseover(selection, view) {
        // show/reveal the tooltip, set its contents,
        // style the element being hovered on
        if (isRecordNA(view, selection.datum()) === false) {
            showTooltip(view);
            setContents(selection.datum(), view);
            setStyle(selection, view);
        }
    }

    function handleMousemove(event) {
        // update the tooltip's position
        const [mouseX, mouseY] = d3.pointer(event, this);
        // add the left & top margin values to account for the SVG g element transform
        setPosition(mouseX, mouseY);
    }

    function handleMouseleave(selection, view) {
        // do things like hide the tooltip
        // reset the style of the element being hovered on
        hideTooltip();
        resetStyle(selection, view);
    }

    function showTooltip(view) {

        tooltipDiv
            .style("background-color", view === "cond_est comp"? "lightgray": "white")
            .style("display", "block");
    }

    function hideTooltip() {
        tooltipDiv.style("display", "none");
    }

    function setPosition(mouseX, mouseY) {
        let shrink_ratio = (width/dashboard_width);
        tooltipDiv
            .style(
                "top",
                mouseY < options.height / 2 ? 
                `${mouseY * shrink_ratio + MOUSE_POS_OFFSET}px` : "initial"
            )
            .style(
                "right",
                mouseX >= width / 2 ?
                `${width - mouseX * shrink_ratio + MOUSE_POS_OFFSET}px` : "initial"
            )
            .style(
                "bottom",
                mouseY >= options.height / 2 ?
                `${(options.height * shrink_ratio) - (mouseY * shrink_ratio) + MOUSE_POS_OFFSET}px` : "initial"
            )
            .style(
                "left",
                mouseX < width / 2 ? 
                `${mouseX * shrink_ratio + MOUSE_POS_OFFSET}px` : "initial"
            );
    }

    function setStyle(selection, view) {
        selection.attr("cursor", "pointer");
        if (view === "cond_est comp") {
            selection.attr("stroke-opacity", .6);
            selection.style("opacity", .6);
        }
    }

    function resetStyle(selection, view) {
        selection.attr("cursor", "default");
        if (view === "cond_est comp") {
            selection.attr("stroke-opacity", default_stroke_opacity);
            selection.style("opacity", default_stroke_opacity);
        }
    }

    function setContents(d, view) {
        // customize this function to set the tooltip's contents however you see fit
        tooltipDiv
            .html(function() {
                if (view === "cond_est") {
                    return getCondEstTooltip(d);
                } else if (view === "indicator") {
                  if(d['commentText'] !== ""){
                    return getIndicatorTooltip(d);
                      } else {
                        return hideTooltip();
                      }
              //  } else if (view === "cond_est comp") {
              //      return getCondEstTooltip(d);
                } else if (view === "change") {
                  if(d['changeT1.P.Estimate'] !== null){
                    return getChangeTooltip(d);
                      } else{
                        return hideTooltip();
                      }
                } else {
                  if(d['changeT1.P.Estimate'] !== null){
                    return getLongTermChangeTooltip(d);
                  } else{
                        return hideTooltip();
                      }
                }
            });
    }
}
