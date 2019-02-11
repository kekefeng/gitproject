def application(environ, start_response):
    start_response('200 OK', [('Content-Type', 'text/html')])
    test = 'hello world'
    return test.encode("utf-8")
