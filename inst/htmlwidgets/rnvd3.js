HTMLWidgets.widget({
  name: "rnvd3",

  type: "output",

  factory: function (el, width, height) {
    // TODO: define shared variables for this instance

    return {
      renderValue: function (x) {
        var Data = JSON.parse(x.mbcData);

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
              .axisLabelDistance(x.xAxisTitleDistance)
              .fontSize(x.xLabelsFontSize);


            chart.yAxis
              .axisLabel(x.yAxisTitle)
              .axisLabelDistance(x.yAxisTitleDistance)
              .fontSize(x.yLabelsFontSize)
              .showMaxMin(x.xAxisShowMaxMin)
              .tickFormat(d3.format(x.xAxisTickFormat));


            d3.select(el.firstElementChild).datum(Data).call(chart);

            nv.utils.windowResize(chart.update);

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
