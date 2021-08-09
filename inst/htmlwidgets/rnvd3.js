HTMLWidgets.widget({
  name: "rnvd3",

  type: "output",

  factory: function (el, width, height) {
    // TODO: define shared variables for this instance
    console.log("el", el);

    return {
      renderValue: function (x) {
        // TODO: code to render the widget, e.g.
        var Data = JSON.parse(x.mbcData);
        console.log("Data", Data);

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
            .groupSpacing(x.groupSpacing);

          chart.reduceXTicks(false).staggerLabels(false);

          chart.xAxis
            .axisLabel(x.xAxisTitle)
            .axisLabelDistance(x.xAxisLabelDistance);

          chart.yAxis
            .axisLabel(x.yAxisTitle)
            .axisLabelDistance(x.yAxisLabelDistance)
            .showMaxMin(x.yAxisShowMaxMin)
            .tickFormat(d3.format(x.yAxisTickFormat));

          d3.select(el.firstElementChild).datum(Data).call(chart);

//          d3.select(selector).on("mouseout", function () {
//            d3.selectAll(".nvtooltip").hide();
//          });

          nv.utils.windowResize(chart.update);

          return chart;
        });
      },

      resize: function (width, height) {
        // TODO: code to re-render the widget with a new size
      }
    };
  }
});
