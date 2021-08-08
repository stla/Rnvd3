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
      },

      resize: function (width, height) {
        // TODO: code to re-render the widget with a new size
      }
    };
  }
});
