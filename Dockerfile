FROM r-base

COPY . ./

RUN installGithub.r MarkEdmondson1234/googleAuthR
