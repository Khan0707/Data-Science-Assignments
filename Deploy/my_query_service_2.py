
from flask_restful import Resource, reqparse
from flask import request

class QueryService(Resource):
    def post(self):
        arg1 = parser.parse_args()
        result = clf.predict(arg1["data"])
        return result


class QueryAnalyzer(object):
    

    def predict(self, data):
        try:
            if "sales" in data.lower():
                response = "Your sales is 10,000"  ### here it should be coming fro excel .
                return response
            elif "attire" in data.lower():
                return "Shirts were sold most"

            elif "discounted item" in data.lower():
                return "50 discounted  items were sold"
            elif "average count" in data.lower():
                return "28 were sold"

            return "Please try again"
        except Exception as e:
            return {"phrase": "Sorry, something unexpected happened.", "original_exception": e.message}, False

parser = reqparse.RequestParser()
parser.add_argument("data")
clf = QueryAnalyzer()
