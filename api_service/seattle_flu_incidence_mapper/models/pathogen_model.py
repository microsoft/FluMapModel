from datetime import datetime

from seattle_flu_incidence_mapper.orm_config import db
from seattle_flu_incidence_mapper.utils import ma


class PathogenModel(db.Model):
    __tablename__ = 'pathogen_model'
    id = db.Column(db.String,  primary_key=True)
    name = db.Column(db.String)
    query_str = db.Column(db.String)
    model_type = db.Column(db.String)
    created = db.Column(db.DateTime, primary_key=True,
                          default=datetime.utcnow)


class PathogenModelSchema(ma.ModelSchema):
    class Meta:
        model = PathogenModel
        sqla_session = db.session