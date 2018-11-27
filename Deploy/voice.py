from flask import Flask, render_template,request,redirect, url_for,send_from_directory
from my_query_service_2 import QueryService
from flask_restful import Api, Resource, reqparse

# create the application object
app = Flask(__name__)

# use decorators to link the function to a url
@app.route('/', methods=['GET', 'POST'])
def home():
    error = None
    if request.method == 'POST':
        if request.form['username'] != 'Landmark' or request.form['password'] != '123':
            error = 'Invalid Credentials. Please try again.'
        else:
            return redirect(url_for('welcome'))
    return render_template('login.html', error=error)


#@app.route('/welcome')
#def welcome():
    #return render_template('welcome.html')  # render a template



#app = Flask(__name__)
api = Api(app)
api.add_resource(QueryService, '/lm_query')


#@app.route("/")
@app.route("/welcome")
def welcome():
    return render_template("index.html")


@app.route("/favicon.ico")
def favicon():
    from os import path
    return send_from_directory(path.join(app.root_path, "static"), "favicon.ico", mimetype = "image/vnd.microsoft.icon")
    return None


@app.after_request
def after_request(response):
    response.headers.add('Access-Control-Allow-Origin', '*')
    response.headers.add('Access-Control-Allow-Headers', 'Content-Type,Authorization')
    response.headers.add('Access-Control-Allow-Methods', 'GET,PUT,POST,DELETE')
    return response

if __name__ == '__main__':
    app.run(debug=True)