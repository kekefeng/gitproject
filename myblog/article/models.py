from django.db import models

# Create your models here.


class Tag(models.Model):
    tag = models.CharField(u'标签',max_length=256)

    def __str__(self):
        return self.tag

    class Meta:
        verbose_name = "标签"

class Article(models.Model):
    title = models.CharField(u'标题', max_length=256)
    content = models.TextField(u'内容')
    summary = models.TextField(u'摘要')
    tag = models.ManyToManyField(Tag, related_name='news_image')
    pub_date = models.DateTimeField(u'发表时间', auto_now_add=True, editable=True)

    def __str__(self):  # 在Python3中用 __str__ 代替 __unicode__
        return self.title

    class Meta:
        verbose_name = "文章"
