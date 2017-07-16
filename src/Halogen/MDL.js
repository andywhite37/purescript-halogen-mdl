exports.upgradeElement = function(element) {
  return function() {
    try {
      console.log("upgradeElement: " + element.classList);
      componentHandler.upgradeElement(element);
    } catch (e) {
      console.error('Failed to upgradeElement', element, e);
    }
    return {};
  };
};

exports.removeClass = function(element) {
  return function(className) {
    element.classList.remove(className);
    return {};
  };
};
