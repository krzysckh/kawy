OL=ol-small

all: public/index.html
public/index.html: build.scm
	$(OL) -i third-party/robusta -r build.scm
clean:
	rm -f public/index.html
