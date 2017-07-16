// Nasty hack to hide the layout drawer
// https://stackoverflow.com/questions/31536467/how-to-hide-drawer-upon-user-click/37625650#37625650
exports.hideLayoutDrawer = function() {
  //return function() {
    var layout = document.querySelector('.mdl-layout');
    if (layout && layout.MaterialLayout) {
      layout.MaterialLayout.toggleDrawer();
    }
    return {};
  //};
};
