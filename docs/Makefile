# Change this to whatever lisp you'r using
LISP=sbcl
EVAL=--eval
QUIT=(uiop:quit)
SYSTEM=qbook

docs: pdf html

html:
	mkdir -p html/
	${LISP} ${EVAL} "(asdf:oos 'asdf:load-op :qbook)" \
	        ${EVAL} "(asdf:oos 'asdf:load-op :${SYSTEM})" \
		    ${EVAL} "(qbook:publish-system-qbook :qbook 'qbook:html-generator \
                                           :output-directory \"./html/\" \
                                           :title \"${SYSTEM}\")" \
		    ${EVAL} "${QUIT}"

pdf:
	mkdir -p pdf/
	${LISP} ${EVAL} "(asdf:oos 'asdf:load-op :qbook)" \
            ${EVAL} "(asdf:oos 'asdf:load-op :${SYSTEM})" \
		    ${EVAL} "(qbook:publish-system-qbook :qbook 'qbook:latex-generator \
                                            :output-file \"./pdf/${SYSTEM}.tex\" \
                                            :title \"${SYSTEM}\")" \
		    ${EVAL} "${QUIT}"
	(cd pdf && pdflatex -shell-escape ${SYSTEM}.tex)
	(cd pdf && makeindex ${SYSTEM}.idx)
	(cd pdf && pdflatex -shell-escape ${SYSTEM}.tex)
	rm pdf/${SYSTEM}.aux pdf/${SYSTEM}.log pdf/${SYSTEM}.toc pdf/${SYSTEM}.tex

clean:
	rm -rf html pdf
