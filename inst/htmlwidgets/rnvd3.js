function getStateForShiny(state, keys) {
  var disabled = {};
  for (var i = 0; i < keys.length; i++) {
    disabled[keys[i]] = state.disabled[i];
  }
  return { stacked: state.stacked, disabled: disabled };
}

var inShiny = HTMLWidgets.shinyMode;

HTMLWidgets.widget({
  name: "rnvd3",

  type: "output",

  factory: function (el, width, height) {
    var id_state = el.id + "_state";

    return {
      renderValue: function (x) {
        var Data = JSON.parse(x.Data);
        var keys = Data.map(function (x) {
          return x.key;
        });

        if (inShiny) {
          var disabled = {};
          for (var i = 0; i < keys.length; i++) {
            disabled[keys[i]] = false;
          }
          var shinyState = { stacked: false, disabled: disabled };
          Shiny.setInputValue(id_state, shinyState);
        }

        if (x.chart === "multibarchart") {
          /* ------------------------ multibarchart ------------------------- */
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
              .reduceXTicks(x.reduceXticks)
              .staggerLabels(x.staggerLabels)
              .wrapLabels(x.wrapLabels)
              .useInteractiveGuideline(x.useInteractiveGuideline)
              .clipEdge(false); // ?

            var tfvalue = x.tooltipFormatters.value;
            var tfheader = x.tooltipFormatters.header;
            var tfkey = x.tooltipFormatters.key;
            if (tfvalue) {
              chart.tooltip.valueFormatter(tfvalue);
            }
            if (tfheader) {
              chart.tooltip.headerFormatter(tfheader);
            }
            if (tfkey) {
              chart.tooltip.keyFormatter(tfkey);
            }

            //          if(x.title){
            //            chart.title(x.title).titleOffset(x.titleOffset);
            //          }
            if (x.radioButtonMode) {
              chart.legend.radioButtonMode(true);
              chart.showControls(false);
            }

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

            if (inShiny) {
              chart.dispatch.on("stateChange", function (state) {
                var shinyState = getStateForShiny(state, keys);
                Shiny.setInputValue(id_state, shinyState);
              });
            }

            return chart;
          });
        } else if (x.chart === "horizontalmultibarchart") {
          /* ------------------------ hmultibarcart ------------------------- */
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

            var tfvalue = x.tooltipFormatters.value;
            var tfheader = x.tooltipFormatters.header;
            var tfkey = x.tooltipFormatters.key;
            if (tfvalue) {
              chart.tooltip.valueFormatter(tfvalue);
            }
            if (tfheader) {
              chart.tooltip.headerFormatter(tfheader);
            }
            if (tfkey) {
              chart.tooltip.keyFormatter(tfkey);
            }

            chart.xAxis
              .axisLabel(x.xAxisTitle)
              .axisLabelDistance(x.xAxisTitleDistance)
              .fontSize(x.xLabelsFontSize);

            chart.yAxis
              .axisLabel(x.yAxisTitle)
              .axisLabelDistance(x.yAxisTitleDistance)
              .fontSize(x.yLabelsFontSize)
              .showMaxMin(x.yAxisShowMaxMin)
              .tickFormat(d3.format(x.yAxisTickFormat))
              .ticks(x.nticks);

            d3.select(el.firstElementChild).datum(Data).call(chart);

            nv.utils.windowResize(chart.update);

            return chart;
          });
        } else if (x.chart === "linechart") {
          /* -------------------------- linechart --------------------------- */
          var isDate     = x.isDate;
          var isPOSIXct  = x.isPOSIXct;
          var isDateTime = isDate || isPOSIXct;
          if (isDate) {
            for (var k = 0; k < Data.length; k++) {
              var values = Data[k].values;
              for (var i = 0; i < values.length; i++) {
                var ymd = values[i];
                values[i].x = new Date(ymd.year, ymd.month - 1, ymd.day);
              }
            }
            if(x.xRange){
              var x1 = x.x1;
              var x2 = x.x2;
              x.xRange[0] = new Date(x1.year, x1.month - 1, x1.day);
              x.xRange[1] = new Date(x2.year, x2.month - 1, x2.day);
            }
          }else if(isPOSIXct) {
            for (var k = 0; k < Data.length; k++) {
              var values = Data[k].values;
              for (var i = 0; i < values.length; i++) {
                var v = values[i];
                values[i].x = new Date(
                  v.year, v.month - 1, v.day, v.hour, v.minute, v.second
                );
              }
            }
            if(x.xRange){
              var x1 = x.x1;
              var x2 = x.x2;
              x.xRange[0] = new Date(
                x1.year, x1.month - 1, x1.day, x1.hour, x1.minute, x1.second
              );
              x.xRange[1] = new Date(
                x2.year, x2.month - 1, x2.day, x2.hour, x2.minute, x2.second
              );
            }
          }

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
              .legendPosition(x.legendPosition)
              .rightAlignYAxis(x.rightAlignYaxis);

            if (x.xRange) {
              chart.xDomain(x.xRange);
            }
            if (x.yRange) {
              chart.yDomain(x.yRange);
            }

            var tfvalue = x.tooltipFormatters.value;
            var tfheader = x.tooltipFormatters.header;
            var tfkey = x.tooltipFormatters.key;
            if (tfvalue) {
              chart.tooltip.valueFormatter(tfvalue);
            }
            if (tfheader) {
              chart.tooltip.headerFormatter(tfheader);
            }
            if (tfkey) {
              chart.tooltip.keyFormatter(tfkey);
            }

            chart.xAxis //Chart x-axis settings
              .axisLabel(x.xAxisTitle)
              .tickFormat(function (d) {
                return isDateTime
                  ? d3.time.format(x.xAxisTickFormat)(new Date(d))
                  : d3.format(x.xAxisTickFormat)(d);
              })
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
        } else if (x.chart === "linefocuschart") {
          /* ----------------------- linefocuschart ------------------------- */
          var isDate     = x.isDate;
          var isPOSIXct  = x.isPOSIXct;
          var isDateTime = isDate || isPOSIXct;
          if (isDate) {
            for (var k = 0; k < Data.length; k++) {
              var values = Data[k].values;
              for (var i = 0; i < values.length; i++) {
                var ymd = values[i];
                values[i].x = new Date(ymd.year, ymd.month - 1, ymd.day);
              }
            }
            if(x.xRange){
              var x1 = x.x1;
              var x2 = x.x2;
              x.xRange[0] = new Date(x1.year, x1.month - 1, x1.day);
              x.xRange[1] = new Date(x2.year, x2.month - 1, x2.day);
            }
          }else if(isPOSIXct) {
            for (var k = 0; k < Data.length; k++) {
              var values = Data[k].values;
              for (var i = 0; i < values.length; i++) {
                var v = values[i];
                values[i].x = new Date(
                  v.year, v.month - 1, v.day, v.hour, v.minute, v.second
                );
              }
            }
            if(x.xRange){
              var x1 = x.x1;
              var x2 = x.x2;
              x.xRange[0] = new Date(
                x1.year, x1.month - 1, x1.day, x1.hour, x1.minute, x1.second
              );
              x.xRange[1] = new Date(
                x2.year, x2.month - 1, x2.day, x2.hour, x2.minute, x2.second
              );
            }
          }

          nv.addGraph(function () {
            var chart = nv.models
              .lineWithFocusChart()
              .margin(x.margins)
              .useInteractiveGuideline(x.useInteractiveGuideline)
              .duration(x.duration)
              .showLegend(true)
              .showYAxis(true)
              .showXAxis(true)
              .interpolate(x.interpolate)
              .legendPosition(x.legendPosition)
              .rightAlignYAxis(x.rightAlignYaxis);

            if (x.xRange) {
              chart.xDomain(x.xRange);
            }
            if (x.yRange) {
              chart.yDomain(x.yRange);
            }

            var tfvalue = x.tooltipFormatters.value;
            var tfheader = x.tooltipFormatters.header;
            var tfkey = x.tooltipFormatters.key;
            if (tfvalue) {
              chart.tooltip.valueFormatter(tfvalue);
            }
            if (tfheader) {
              chart.tooltip.headerFormatter(tfheader);
            }
            if (tfkey) {
              chart.tooltip.keyFormatter(tfkey);
            }

            chart.xAxis //Chart x-axis settings
              .axisLabel(x.xAxisTitle)
              .tickFormat(function (d) {
                return isDateTime
                  ? d3.time.format(x.xAxisTickFormat)(new Date(d))
                  : d3.format(x.xAxisTickFormat)(d);
              })
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
