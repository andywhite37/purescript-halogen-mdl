exports.change = function(element) {
  console.log('change 1');
  return function(value) {
    console.log('change 2');
    return function() {
      console.log('change 3');
      console.log(element);
      if (element && element.MaterialSlider && element.MaterialSlider.change) {
        console.log('change ' + value);
        element.MaterialSlider.change(value);
      } else {
        console.log('no change');
      }
      return {};
    };
  };
};
