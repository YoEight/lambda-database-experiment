# Lambda Database Experiment (LDE)

Experimental event store database entirely written in Haskell. The main goal is learning. Database programming is full of interesting algorithms and challenges. There is no plan for going production ready but it will be definitely cool if
it happens someday.

This project is comprised of:
* **lambda-bus**: In-memory message bus. It's used in **lambda-node** and **lambda-client** and helps to implement a **S**taged **E**vent-**D**riven **A**rchitecture.
* **lambda-client**: TCP client of **lambda-node**.
* **lambda-logger**: Logging infrastructure used by **lambda-bus**, **lambda-client**, **lambda-node** and **lambda-prelude**.
* **lambda-node**: Eventstore database server.
* **lambda-prelude**: A prelude specific to **LDE** project.
* **lambda-protocol**: Gathers all the type declarations common to **lambda-client** and **lambda-node**.

## How to build this project ?

This project assumes a 64bits Unix system and the build tool [stack][] installed. The project is developped mainly on Mac OSX and Linux based distributions. For now, nothing prevents the project from being built on Windows. That being said, Windows will **never** be officially supported.

To build the entire project:

```
$ stack build
```

You can also build a specific package by appending its name to the build command.

```
$ stack build lambda-node
```

## Notes

Contributions and bug reports are welcome!

MIT License

-Yorick Laupa

[stack]: http://haskellstack.org
