from django.db import models
from article.models import Article
# Create your models here.

class Comments(models.Model):
    name = models.CharField(max_length=100)
    #email = models.EmailField(max_length=255)
    #url = models.URLField(blank=True)
    text = models.TextField()

    created_time = models.DateTimeField(auto_now_add=True)
    article = models.ForeignKey(Article, on_delete=models.CASCADE)
    
    def __str__(self):
        return self.text[:20]
