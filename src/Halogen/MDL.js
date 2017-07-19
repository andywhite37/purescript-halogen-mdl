exports.upgradeElement = function(element) {
  return function() {
    try {
      console.log("MDL.upgradeElement: " + element.classList);
      componentHandler.upgradeElement(element);
    } catch (e) {
      console.error('MDL.upgradeElement: failed to upgradeElement', element, e);
    }
    return {};
  };
};

exports.upgradeElementsBySelector = function(selector) {
  return function() {
    try {
      console.log("MDL.upgradeElementsBySelector " + selector);
      var elements = document.querySelectorAll(selector);
      if (elements && elements.length > 0) {
        elements.forEach(function(element) {
          componentHandler.upgradeElement(element);
        });
      }
    } catch (e) {
      console.error('MDL.upgradeElementsBySelector: failed to upgradeElements by selector', selector, e);
    }
    return {};
  };
};

exports.upgradeElementsBySelectors = function(selectors) {
  return function() {
    try {
      console.log("MDL.upgradeElementsBySelectors " + selectors);
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
      console.error('MDL.upgradeElementsBySelectors: failed to upgradeElements by selectors', selectors, e);
    }
    return {};
  };
};

exports.removeClass = function(element) {
  return function(className) {
    console.log("MDL.removeClass: " + className);
    element.classList.remove(className);
    return {};
  };
};
