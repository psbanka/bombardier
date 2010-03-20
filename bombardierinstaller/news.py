import os
from google.appengine.ext.webapp import template
from google.appengine.api import images

import cgi

from google.appengine.api import users
from google.appengine.ext import webapp
from google.appengine.ext.webapp.util import run_wsgi_app
from google.appengine.ext import db

AUTHORIZED_USERS = ["peter.banka"]

class NewsItem(db.Model):
    author = db.UserProperty()
    content = db.TextProperty()
    date = db.DateTimeProperty(auto_now_add=True)
    picture = db.BlobProperty()
    title = db.StringProperty()

class Image (webapp.RequestHandler):
    def get(self):
      news_item = db.get(self.request.get("img_id"))
      if news_item.picture:
          self.response.headers['Content-Type'] = "image/png"
          self.response.out.write(news_item.picture)
      else:
          self.error(404)

class Thumbnail (webapp.RequestHandler):
    def get(self):
      news_item = db.get(self.request.get("img_id"))
      if news_item.picture:
          self.response.headers['Content-Type'] = "image/png"
          thumbnail = images.resize(news_item.picture, 64, 64)
          self.response.out.write(thumbnail)
      else:
          self.error(404)

class BasePage(webapp.RequestHandler):
    def get(self):
        super_user = False
        news_query = NewsItem.all().order('-date')
        news_items = news_query.fetch(10)

        user_name = users.get_current_user()
        if user_name:
            url = users.create_logout_url(self.request.uri)
            url_linktext = 'Logout'
            if str(user_name) in AUTHORIZED_USERS:
                super_user = True
        else:
            url = users.create_login_url(self.request.uri)
            url_linktext = 'Login'

        self.template_values = {
            'news_items': news_items,
            'url': url,
            'url_linktext': url_linktext,
            'user_name': user_name,
            'super_user': super_user,
            }

class MainPage(BasePage):
    def get(self):
        super(MainPage, self).get()
        path = os.path.join(os.path.dirname(__file__), 'front_page.html')
        self.response.out.write(template.render(path, self.template_values))

class Story(BasePage):
    def get(self):
        super(Story, self).get()
        news_item = db.get(self.request.get("story_id"))
        self.template_values["news_item"] = news_item

        path = os.path.join(os.path.dirname(__file__), 'story.html')
        self.response.out.write(template.render(path, self.template_values))

class Delete(BasePage):
    def get(self):
        user_name = users.get_current_user()
        if not str(user_name) in AUTHORIZED_USERS:
            self.error(501)
        else:
            news_item = db.get(self.request.get("story_id"))
            news_item.delete()
            self.redirect('/')

class AddNews(BasePage):
    def get(self):
        super(AddNews, self).get()
        user_name = users.get_current_user()
        if not str(user_name) in AUTHORIZED_USERS:
            self.error(501)
        else:
            path = os.path.join(os.path.dirname(__file__), 'new_post.html')
            self.response.out.write(template.render(path, {}))

    def post(self):
        news_item = NewsItem()
        if users.get_current_user():
            news_item.author = users.get_current_user()

        news_item.title = self.request.get("title")
        if self.request.get("img"):
            news_item.picture = db.Blob(self.request.get("img"))
        news_item.content = self.request.get('content')
        news_item.put()
        self.redirect('/')


def main():
    application = webapp.WSGIApplication([('/', MainPage),
                                          ('/img', Thumbnail),
                                          ('/story', Story),
                                          ('/delete', Delete),
                                          ('/add_news', AddNews)],
                                         debug=True)
    run_wsgi_app(application)

if __name__ == "__main__":
    main()
