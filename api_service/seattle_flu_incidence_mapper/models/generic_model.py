from datetime import datetime
from sqlalchemy import String, Column, DateTime
from seattle_flu_incidence_mapper.orm_config import get_session, get_declarative_base
from seattle_flu_incidence_mapper.utils import ma

base = get_declarative_base()


class GenericModel(base):
    __tablename__ = 'generic_model'
    id = Column(String,  primary_key=True)
    name = Column(String)
    query_str = Column(String)
    model_type = Column(String)
    model_key = Column(String, primary_key=True)
    rds_key = Column(String)
    created = Column(DateTime, default=datetime.utcnow)


class GenericModelSchema(ma.ModelSchema):
    class Meta:
        model = GenericModel
        sqla_session = get_session
