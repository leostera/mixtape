load("//tools/build_rules:erlang.bzl", "erlang_application")

erlang_application(
  visibility = ["//service:__subpackages__"],
  name = "mixtape",
  app_src = "mixtape.app.src",
  srcs = glob(["*.erl"]),

  deps = [
    # "//3rdparty/beam/github.com/ninenines:cowboy",
    # "//3rdparty/beam/github.com/davisp:jiffy",
  ]
)
