FROM httpd

RUN sed -i -r 's/#(LoadModule proxy_module )/\1/' conf/httpd.conf
RUN sed -i -r 's/#(LoadModule proxy_http_module )/\1/' conf/httpd.conf
RUN echo 'Include conf/backend.conf' >> conf/httpd.conf

COPY backend.conf conf/backend.conf
