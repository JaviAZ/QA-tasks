from application import db
db.drop_all()
from application.models import Users, Posts
db.create_all()