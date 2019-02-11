//提交评论
$(function(){
  $('.comment-btn').on('click', function(){
      jQuery.ajax({
          cache: false,
          type: "POST",
          url:"/comment",
          data:$('.comment-form').serialize(),
          dateType:"json",
          async: true,
          beforeSend:function(xhr, settings){
          xhr.setRequestHeader("X-CSRFToken", $('#csrf_token').val());
      },
          success: function(data) {
              $('input').val("");
              $('textarea').val("");
              $('#successpost').show();
              if(data.status == 'success'){
                   window.location.reload();//刷新当前页面.
              }else if(data.status == 'fail'){
                  alert("评论不能为空，请重新评论");
              }
          },
      });
  });
})

//提交新文章
$(function(){
    $('.but').on('click', function(){
        var gnl = confirm("确定要提交？");
        if (gnl == true){
            $.post("../../newarticle",
                {
                    title : $('#title').val(),
                    content : $('#content').val(),
                    tags : $('#tags-input').val(),
                    types : $('#types').val(),
                    article_id: $('.article_id').text(),
                },
                function(data,status){
                    alert("提交成功！");
                    window.location.href = "../../"
                },
                "json"
            );
        }else{
            return false;
        }
    });
})

$(function(){
    $('.single-tag').on('click', function(){
        tagstext = $('#tags-input').val()
        add_tag = $(this).html()
        newtext = tagstext + " " + add_tag + " "
        $('#tags-input').val(newtext)
    });
})

//编辑功能
$(function(){
    $('.edit').on('click', function(){
        article_id = $('.article_id').text();
        window.open("../editor/" + article_id);     
    })
})