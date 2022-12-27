# The .app file
This is the source file for the Erlang .app file.

This is used by OTP to package start and package the system.

On compilation this .src file is taken and populated with all the modules in the application

(Compare the .app file in the ebin/ under _build/)

```erlang
{application, belka_example,
 [{description, "An OTP Gemini Server"},
  {vsn, "0.1.0"},
  {registered, []},
  {mod, {belka_example, []}},
  {applications,
   [kernel,
    stdlib
   ]},
  {env,[]},
  {modules, []},

  {licenses, ["GNU GPL V3"]},
  {links, []}
 ]}.

```
