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

exports.upgradeElementsBySelector = function(selector) {
  return function() {
    try {
      console.log("upgradeElementsBySelector " + selector);
      var elements = document.querySelectorAll(selector);
      if (elements && elements.length > 0) {
        elements.forEach(function(element) {
          componentHandler.upgradeElement(element);
        });
      }
    } catch (e) {
      console.error('Failed to upgradeElements by selector', selector, e);
    }
    return {};
  };
};

exports.upgradeElementsBySelectors = function(selectors) {
  return function() {
    try {
      console.log("upgradeElementsBySelectors " + selectors);
      if (selectors && selectors.length > 0) {
        selectors.forEach(function(selector) {
          var elements = document.querySelectorAll(selector);
          if (elements && elements.length > 0) {
            elements.forEach(function(element) {
              componentHandler.upgradeElement(element);
            });
          }
        });
      }
    } catch (e) {
      console.error('Failed to upgradeElements by selector', selector, e);
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
