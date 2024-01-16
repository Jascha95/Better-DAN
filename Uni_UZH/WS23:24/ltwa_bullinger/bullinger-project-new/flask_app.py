import os
import psycopg2

from flask import Flask, jsonify, send_from_directory

app = Flask(__name__)

# Create a list to store submitted messages
messages = []
# Use Flask's configuration handling for environment variables
app.config.from_pyfile('variables.env')

# Make a connection to the database
connection = psycopg2.connect(
    host=app.config["DB_HOST"],
    port=app.config["DB_PORT"],
    dbname=app.config["DB_NAME"],
    user=app.config["DB_USER"],
    password=app.config["DB_PASSWORD"]
)
cursor = connection.cursor()

@app.route("/lexical-footnotes", methods=["GET"])
def index():
    """ Return the index page """
    select_query = """
        SELECT *
        FROM lexical_footnotes;
    """
    cursor.execute(select_query)
    messages = cursor.fetchall()
    return jsonify(messages)

# Route to serve index.html for all other cases
@app.route('/', defaults={'path': ''})
@app.route('/<path:path>')
def serve(path):
    if path != "lexical-footnotes":
        # Assuming your Vue.js app is in the 'client' folder
        return send_from_directory('client', 'index.html')


if __name__ == "__main__":
    port = int(os.environ.get("PORT", 5000))
    app.run(host="127.0.0.1", port=port, debug=True)
