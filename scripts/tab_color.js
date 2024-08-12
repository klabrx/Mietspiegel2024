$(document).on('click', '.collapse.show', function(e) {
  $('.collapse').not(this).collapse('hide');
});

Shiny.addCustomMessageHandler('updateTabColor', function(message) {
  $('#' + message.tabId).removeClass('btn-light-red').addClass('btn-light-green');
});
