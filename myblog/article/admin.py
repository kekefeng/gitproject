from django.contrib import admin
from .models import Tag,Article
# Register your models here.

admin.site.register(Article)
admin.site.register(Tag)