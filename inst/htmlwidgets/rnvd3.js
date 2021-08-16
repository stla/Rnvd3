HTMLWidgets.widget({
  name: "rnvd3",

  type: "output",

  factory: function (el, width, height) {
    // TODO: define shared variables for this instance

    return {
      renderValue: function (x) {
        var Data = JSON.parse(x.Data);

        if (x.chart === "multibarchart") {
          nv.addGraph(function () {
            var chart = nv.models
              .multiBarChart()
              .x(function (d) {
                return d.label;
              })
              .y(function (d) {
                return d.value;
              })
              .duration(x.duration)
              .margin(x.margins)
              .rotateLabels(x.rotateLabels)
              .groupSpacing(x.groupSpacing)
              .rightAlignYAxis(x.rightAlignYaxis)
              .reduceXTicks(false)
              .staggerLabels(x.staggerLabels)
              .wrapLabels(x.wrapLabels)
              .useInteractiveGuideline(x.useInteractiveGuideline)
              .clipEdge(false); // ?
            //          if(x.title){
            //            chart.title(x.title).titleOffset(x.titleOffset);
            //          }

            chart.xAxis
              .axisLabel(x.xAxisTitle)
              .axisLabelDistance(x.xAxisTitleDistance)
              .fontSize(x.xLabelsFontSize);

            chart.yAxis
              .axisLabel(x.yAxisTitle)
              .axisLabelDistance(x.yAxisTitleDistance)
              .fontSize(x.yLabelsFontSize)
              .showMaxMin(x.yAxisShowMaxMin)
              .tickFormat(d3.format(x.yAxisTickFormat));

            d3.select(el.firstElementChild).datum(Data).call(chart);

            //          d3.select(selector).on("mouseout", function () {
            //            d3.selectAll(".nvtooltip").hide();
            //          });

            nv.utils.windowResize(chart.update);

            if (x.legendTitle) {
              d3.select(".nv-legend g")
                .append("g")
                .attr("transform", "translate(" + x.legendHjust + ",5)")
                .append("text")
                .attr("dy", ".32em")
                .attr("dx", 8)
                .attr("class", "nv-legend-text")
                .attr("text-anchor", "end")
                .text(x.legendTitle);
            }

            return chart;
          });
        } else if (x.chart === "horizontalmultibarchart") {
          nv.addGraph(function () {
            var chart = nv.models
              .multiBarHorizontalChart()
              .x(function (d) {
                return d.label;
              })
              .y(function (d) {
                return d.value;
              })
              .duration(x.duration)
              .margin(x.margins)
              .showValues(x.showValues)
              //              .rotateLabels(x.rotateLabels)
              .groupSpacing(x.groupSpacing);
            //              .rightAlignYAxis(x.rightAlignYaxis)
            //              .reduceXTicks(false)
            //              .staggerLabels(x.staggerLabels)
            //              .wrapLabels(x.wrapLabels)
            //              .useInteractiveGuideline(x.useInteractiveGuideline)
            //              .clipEdge(false); // ?

            chart.xAxis
              .axisLabel(x.xAxisTitle)
              .axisLabelDistance(x.yAxisTitleDistance)
              .fontSize(x.yLabelsFontSize);

            chart.yAxis
              .axisLabel(x.yAxisTitle)
              .axisLabelDistance(x.xAxisTitleDistance)
              .fontSize(x.xLabelsFontSize)
              .showMaxMin(x.xAxisShowMaxMin)
              .tickFormat(d3.format(x.xAxisTickFormat));

            d3.select(el.firstElementChild).datum(Data).call(chart);

            nv.utils.windowResize(chart.update);

            return chart;
          });
        } else if (x.chart === "linechart") {
          nv.addGraph(function () {
            var chart = nv.models
              .lineChart()
              .margin(x.margins)
              .useInteractiveGuideline(x.useInteractiveGuideline) //We want nice looking tooltips and a guideline!
              .duration(x.duration) //how fast do you want the lines to transition?
              .showLegend(true) //Show the legend, allowing users to turn on/off line series.
              .showYAxis(true) //Show the y-axis
              .showXAxis(true) //Show the x-axis
              .interpolate(x.interpolate)
              .legendPosition(x.legendPosition);

            chart.xAxis //Chart x-axis settings
              .axisLabel(x.xAxisTitle)
              .tickFormat(d3.format(x.xAxisTickFormat))
              .fontSize(x.xLabelsFontSize);

            chart.yAxis //Chart y-axis settings
              .axisLabel(x.yAxisTitle)
              .tickFormat(d3.format(x.yAxisTickFormat))
              .fontSize(x.yLabelsFontSize);

            /* Done setting the chart up? Time to render it!*/

            d3.select(el.firstElementChild)
              .datum(Data) //Populate the <svg> element with chart data...
              .call(chart); //Finally, render the chart!

            //Update the chart when window resizes.
            nv.utils.windowResize(function () {
              chart.update();
            });
            return chart;
          });
        }
      },

      resize: function (width, height) {
        // TODO: code to re-render the widget with a new size
      }
    };
  }
});
