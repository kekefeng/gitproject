from bottle import Bottle, run, template, route

app = Bottle()

#@app.route('/hello/<name:re:[a-z]+>/zzx/kk/<id:int>')
@app.route('/hello/<name:int>')
def greet(name='Stranger'):
    return template('Hello {{name}}, how are you?', name=name)

run(app, host='localhost', port=5000, server='wsgiref')
