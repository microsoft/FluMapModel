from config import db, ma
import datetime

class PathoGenModel(db.Model):
    __tablename__ = 'pathogen_model'
    id = db.Column(db.String,  primary_key=True)
    name = db.Column(db.String)
    query = db.Column(db.String)
    latent = db.Column(db.Bool)
    created = db.Column(db.DateTime, 
                          default=datetime.utcnow, 
                          onupdate=datetime.utcnow)

class PathoGenModelSchema(ma.ModelSchema):
    class Meta:
        model = PathoGenModel
        sqla_session = db.session