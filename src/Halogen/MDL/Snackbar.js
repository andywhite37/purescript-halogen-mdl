exports.showSnackbarNoAction = function(element) {
  return function(data) {
    element.MaterialSnackbar.showSnackbar(data);
    return {};
  };
};

exports.showSnackbarWithAction = function(element) {
  return function(data) {
    element.MaterialSnackbar.showSnackbar(data);
    return {};
  };
};

exports.hideSnackbar = function(element) {
  return function() {
    element.MaterialSnackbar.hideSnackbar();
    return {};
  };
};
