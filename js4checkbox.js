$(document).ready(function(){
  $('input[name=numSelector]').on('click', function(event){
    if($('input[name=numSelector]checked').length  3){
      $(this).prop('checked', false);
    }
  });
  $('input[name=numSelector]').on('click', function(event){
    if($('input[name=numSelector]checked').length == 0){
      $(this).prop('checked', true);
    }
  });
});