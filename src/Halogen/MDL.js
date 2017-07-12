exports.upgradeElement = function(element) {
  return function() {
    try {
      componentHandler.upgradeElement(element);
    } catch (e) {
      console.error('Failed to upgradeElement', element, e);
    }
    return {};
  };
};
