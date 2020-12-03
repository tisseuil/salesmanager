
function mod_ticket_js(ns_prefix) {
  
  $("#" + ns_prefix + "ticket_table").on("click", ".delete_btn", function() {
    Shiny.setInputValue(ns_prefix + "ticket_id_to_delete", this.id, { priority: "event"});
    $(this).tooltip('hide');
  });
  
  $("#" + ns_prefix + "ticket_table").on("click", ".edit_btn", function() {
    Shiny.setInputValue(ns_prefix + "ticket_id_to_edit", this.id, { priority: "event"});
    $(this).tooltip('hide');
  });
}
