exports.registerDialog = function(dialogElement) {
  return function() {
    console.log("Dialog.registerDialog");
    if (!dialogElement.showModal) {
      console.log("Dialog.showDialog - attempting dialog polyfill");
      dialogPolyfill.registerDialog(dialogElement);
    }
    return {};
  };
};

exports.showDialog = function(dialogElement) {
  return function() {
    console.log("Dialog.showDialog");
    if (!dialogElement.showModal) {
      console.log("Dialog.showDialog - attempting dialog polyfill");
      dialogPolyfill.registerDialog(dialogElement);
    }
    dialogElement.showModal();
    return {};
  };
};

exports.close = function(dialogElement) {
  return function() {
    console.log("Dialog.close");
    dialogElement.close();
    return {};
  };
};
