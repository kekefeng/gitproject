from django.shortcuts import render, redirect
from .models import Article, Tag
from comments.models import Comments
from django.http import HttpResponse
# Create your views here.
from random import sample
import markdown
from django.template.loader import get_template 
from django.core.paginator import Paginator, EmptyPage, PageNotAnInteger
from comments.views import CommentForm

from django.contrib.auth.models import User
from django.contrib import auth
from django.contrib.auth.decorators import login_required


def index(request):
    articles = Article.objects.all().order_by('-pub_date')
    hots = Article.objects.order_by('?')[:10]

    article_list = get_askpage_articles(articles, request.GET.get('page'))

    return render(request, 'index.html', locals())


def article(request, article_id):
    template = get_template('article.html')
    article = Article.objects.get(id=article_id) #查询指定类型的文章
    content = article.content

    hots = Article.objects.order_by('?')[:10]
    comments = Comments.objects.filter(article=article_id)

    html = template.render({
        'comments': comments,
        'hots': hots,
        'article': article,
        'form': CommentForm(),
        'content': markdown.markdown(
            content,
            extensions=[
                'markdown.extensions.extra',
                'markdown.extensions.codehilite',
                'markdown.extensions.toc',
            ]
        ),

    })
    #previous_blog
    #last_blog
    return HttpResponse(html)

def search(request):
    keyword = request.GET.get('search') #获取搜索的关键词
    articles = Article.objects.filter(title__icontains=keyword)#获取到搜索关键词通过标题进行匹配
    #remen = Article.objects.filter(tui__id=2)[:6]
    #allcategory = Category.objects.all()
    #tags = Tag.objects.all()

    article_list = get_askpage_articles(articles, request.GET.get('page'))
    return render(request, 'search.html', locals())

def get_askpage_articles(articles, ask_page):
    paginator = Paginator(articles, 10)
    try:
        article_list = paginator.page(ask_page)
    except PageNotAnInteger:
        article_list = paginator.page(1)
    except EmptyPage:
        article_list = paginator.page(paginator.num_pages)

    return article_list

def login(request):
    if request.method == 'POST':
        username = request.POST.get('username')
        password = request.POST.get('password')
        re = auth.authenticate(username=username, password=password)
        if re is not None:
            auth.login(request, re)
            return redirect('/')
        else:
            return render(request, 'login.html', {'login_error':'用户名或密码错误'})
    return render(request, 'login.html')

def logout(request):
    auth.logout(request)
    return redirect('/')

@login_required
def editor(request, article_id):
    tags = Tag.objects.all()
    if article_id == 0:  # 新文章
        return render(request, 'editor.html', locals())
    else: # 修改已有文章
        article = Article.objects.filter(id=article_id)
        title = article[0].title
        content = article[0].content
        tag = article[0].tag
        return render(request, 'editor.html', locals())

@login_required
def newarticle(request):
    title = request.POST['title']
    content = request.POST['content']
    tags = request.POST['tags']
    article_id = request.POST['article_id']

    if int(article_id) > 0:
        article = Article.objects.filter(id=article_id)[0]
    else:    
        article = Article()
        
    article.title = title
    article.content = content
    article.save()

    
    tags = tags.strip().split()
    for tag in tags:
        tagobj, logic = Tag.objects.get_or_create(tag=tag)
        article.tag.add(tagobj)
    


    return HttpResponse('{"status": "success"}', content_type='application/json')

def example(request):
    return render(request, 'transindex.html')
