// Hack to hide the layout drawer (it does not automatically hide on a drawer link click)
// https://stackoverflow.com/questions/31536467/how-to-hide-drawer-upon-user-click/37625650#37625650
exports.toggleDrawer = function() {
  var layout = document.querySelector('.mdl-layout');
  if (layout && layout.MaterialLayout && layout.MaterialLayout.toggleDrawer) {
    layout.MaterialLayout.toggleDrawer();
  }
  return {};
};
