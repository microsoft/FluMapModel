from flask import jsonify
from marshmallow import ValidationError
from werkzeug.exceptions import MethodNotAllowed


class SeattleFluException(Exception):
    pass



def register_error_handlers(app):
    @app.errorhandler(ValueError)
    def unhandle_request(e):
        app.logger.exception(e)
        return jsonify({'message': str(e)}), 400

    @app.errorhandler(ValidationError)
    def unhandled_exception(e):
        app.logger.exception(e)
        if isinstance(e.messages, list) and len(e.field_names) > 0:
            e.messages = {field: e.messages for field in e.field_names}
        return jsonify({'messages': e.messages}), 400

    @app.errorhandler(Exception)
    def unhandled_exception(e):
        if isinstance(e, (SeattleFluException, )):
            return jsonify({'message': str(e)}), 400
        elif isinstance(e, (MethodNotAllowed, )):
            return jsonify({'message': e.description + ', '.join(e.valid_methods)}), 405
        app.logger.exception(e)
        return jsonify({'message': 'Server error. Please contact support'}), 500