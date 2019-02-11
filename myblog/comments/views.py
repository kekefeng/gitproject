from django.shortcuts import render
from django.http import HttpResponseRedirect, HttpResponse

from django.utils.html import escape, strip_tags
# Create your views here.
from django.shortcuts import render, get_object_or_404, redirect
from article.models import Article

from .models import Comments
from .forms import CommentForm

def post_comment(request):
    if request.method == 'POST':
        article_id = request.POST.get('blog')
        article = get_object_or_404(Article, id=article_id)

        content = strip_tags(request.POST.get('text')).strip()
        name = strip_tags(request.POST.get('name')).strip()
        
        if name == "":
            name = "匿名网友"

        if content == "":
            return HttpResponse('{"status": "fail"}', content_type='application/json')
            
        comment = Comments()
        comment.name = name
        comment.text = content
        comment.article = article
        comment.save()

            #return HttpResponseRedirect("/index/")
        return HttpResponse('{"status": "success"}', content_type='application/json')
    return HttpResponse('{"status": "fail"}', content_type='application/json')