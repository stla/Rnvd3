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
            .duration(1300)
            .margin({ bottom: 100, left: 70 })
            .rotateLabels(0)
            .groupSpacing(0.1);

          chart.reduceXTicks(false).staggerLabels(false);

          chart.xAxis.axisLabel(x.xAxisTitle).axisLabelDistance(35);

          chart.yAxis
            .axisLabel(x.yAxisTitle)
            .axisLabelDistance(-5)
            .showMaxMin(false)
            .tickFormat(d3.format("d"));

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
