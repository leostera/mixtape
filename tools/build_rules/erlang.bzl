"""

Erlang Repositories

Based off:
- Go: https://github.com/bazelbuild/rules_go/blob/master/go/private/go_toolchain.bzl
- Haskell: https://github.com/tweag/rules_haskell/blob/master/haskell/toolchain.bzl

"""

ErlangApplicationInfo = provider(fields = {
    "name": "The name of the application release",
    "app": "The application configuration file",
    "modules": "The list of all application modules",
    })

ErlangModuleInfo = provider(fields = {
    "name": "The name of the erlang module",
    "src": "The erlang source file for this module",
    "beam": "The BEAM bytecode file for this module",
    })

ERLANG_FILETYPES = ["erl"]

BIN_ERLC="erlc"
BIN_ERL="erl"

def get_bin(name, otp_bins):
  if name == BIN_ERLC:
    return otp_bins.files.to_list()[5]
  elif name == BIN_ERL:
    return otp_bins.files.to_list()[3]

def module_info(src):
  return ErlangModuleInfo(
      name = src.label.name,
      beam = src.files.to_list()[0],
      )

###
### Compile erlang source files down to BEAM bytecode
###
def _erlang_module_impl(ctx):
  # brittle but will do for now
  erlc = get_bin(BIN_ERLC, ctx.attr._otp_bins)

  erlfile = ctx.files.src[0]
  beamfile = ctx.actions.declare_file(
    erlfile.basename.replace("erl", "beam")
  )

  arguments = ["-o", beamfile.dirname, "--", erlfile.path]

  ctx.actions.run(
      arguments = arguments,
      env = { "HOME": ctx.workspace_name },
      executable = erlc,
      inputs = depset([erlfile]).union(ctx.attr._otp_bins.files),
      outputs = [beamfile],
      mnemonic = "ErlCompile",
      progress_message = "Compiling {_in} to {out}".format(
        _in=erlfile.path,
        out=beamfile.path
        ),
      )


  runfiles = []
  for dep in ctx.attr.deps:
    runfiles.extend(dep.files.to_list())

  return [
      DefaultInfo(
          files=depset([beamfile]),
          runfiles=ctx.runfiles(files=runfiles)
          ),
      ErlangModuleInfo(
          name=ctx.label.name,
          src=erlfile,
          beam=beamfile,
          )
      ]

erlang_module = rule(
    attrs = {
        "src": attr.label(
            allow_files = ERLANG_FILETYPES,
            mandatory = True,
            ),
        "deps": attr.label_list(
            mandatory = False,
            ),
        "_otp_bins": attr.label(
            default = Label("@sys_erlang//:bin"),
            cfg = "host",
            )
        },
    implementation =_erlang_module_impl
    )

  ###
### Start an Erlang shell preloading some modules
###
def _erlang_shell_impl(ctx):
  erl = get_bin(BIN_ERL, ctx.attr._otp_bins)

  # Modules directories to be added to erlang's codepath
  module_dirs = []

  # Make erlang binaries available at runtime
  files = []
  files.extend(ctx.attr._otp_bins.files.to_list())

  for dep in ctx.attr.deps:
    # Make erlang modules and paths available at runtime
    files.extend(dep.files.to_list())
    files.extend(dep.default_runfiles.files.to_list())

    # Add erlang dirs to codepath (we only need the last bit of the path)
    module_dirs.extend([ f.dirname.split("bin/")[1] for f in dep.files ])
    module_dirs.extend([ m.beam.dirname.split("bin/")[1] for m in dep[ErlangApplicationInfo].modules ])

  modules = depset(module_dirs)

  # Write script to initiate shell, loading the modules
  script = ctx.actions.declare_file("%s-erlang-shell" % ctx.label.name )
  script_content = """#!/bin/sh
# Auto-generated, do not modify!

exec {erl} {deps}
""".format(
    erl=erl.path,
    deps= " ".join(["-pa {0}".format(module) for module in modules ])
  )
  ctx.actions.write(script, script_content, is_executable=True)

  runfiles = ctx.runfiles(files=files)
  return DefaultInfo(executable=script, runfiles=runfiles)

erlang_shell = rule(
    attrs = {
        "deps": attr.label_list(
            mandatory = False,
            ),
        "_otp_bins": attr.label(
            default = Label("@sys_erlang//:bin"),
            cfg = "host",
            )
        },
    implementation = _erlang_shell_impl,
    executable = True,
    )

###
### Build an Erlang Application
###
### It will automatically generate build rules for each one of it's
### src modules.
###
def _erlang_application_impl(ctx):
  beamfiles = ctx.files.srcs
  app_src_file = ctx.files.app_src[0]

  app_file = ctx.actions.declare_file(
      app_src_file.basename.replace(".src","")
      )

  ctx.actions.run_shell(
      command = "mv {src} {dest}".format(
          src=app_src_file.short_path,
          dest=app_file.path
          ),
      inputs=[app_src_file],
      outputs=[app_file],
      )

  tarball = ctx.actions.declare_file("{0}.tar".format(ctx.label.name))

  command = "tar cf {target} {app_src} {srcs}".format(
      srcs=" ".join([ f.path for f in beamfiles]),
      app_src=app_src_file.short_path,
      target=tarball.path
  )

  runfiles = [app_src_file]
  runfiles.extend(beamfiles)

  # Force all dependant files to be compiled by packing their outputs
  ctx.actions.run_shell(
      command = command,
      inputs = runfiles,
      outputs = [tarball],
  )

  return [
      DefaultInfo(
          files=depset([tarball, app_file]),
          runfiles=ctx.runfiles(files=runfiles),
          ),
      ErlangApplicationInfo(
          name=ctx.label.name,
          app=app_file,
          modules=[ module_info(src) for src in ctx.attr.srcs ],
          )
      ]

_erlang_application = rule(
    attrs = {
        "app_src": attr.label(
            allow_files = True,
            mandatory = True,
            ),
        "srcs" : attr.label_list(
            allow_files = True,
            mandatory = True,
            ),
        "deps" : attr.label_list(),
        "_otp_bins": attr.label(
            default = Label("@sys_erlang//:bin"),
            cfg = "host",
            )
        },
    implementation = _erlang_application_impl
    )

def erlang_application(name, app_src, srcs, deps, **kwargs):
  for src in srcs:
    erlang_module(
        name=src.replace(".erl","_erl"),
        src=src,
        **kwargs
        )

  app_src_target = app_src.replace(".app.src","_app_src")

  native.filegroup(
    name=app_src_target,
    srcs=[app_src]
  )

  _erlang_application(
    name=name,
    app_src=app_src_target,
    srcs=[ src.replace(".erl", "_erl") for src in srcs ],
    **kwargs
    )
