

# Get the application instance
from seattle_flu_incidence_mapper.config import app
from seattle_flu_incidence_mapper.jwt import generate_token
if __name__ == "__main__":
    app.cli()
