exports.upgradeElement = function(element) {
  return function() {
    try {
      console.log("upgradeElement: " + element);
      componentHandler.upgradeElement(element);
    } catch (e) {
      console.error('Failed to upgradeElement', element, e);
    }
    return {};
  };
};
