exports.setProgress = function(element) {
  return function(value) {
    if (element && element.MaterialProgress && element.MaterialProgress.setProgress) {
      element.MaterialProgress.setProgress(value);
    }
    return {};
  };
};

exports.setBuffer = function(element) {
  return function(value) {
    if (element && element.MaterialProgress && element.MaterialProgress.setBuffer) {
      element.MaterialProgress.setBuffer(value);
    }
    return {};
  };
};
