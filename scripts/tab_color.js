// tab_color.js
Shiny.addCustomMessageHandler("updateTabColor", function(message) {
  $("#" + message.tabId)
    .removeClass(message.oldClass)
    .addClass(message.newClass);
});
