# First person shooter game in Haskell
However to be honest at current stage it's just a map viewer...

### System Requirements
- OS: Windows/Linux/OSX
- Graphics: OpenGL 3.3 or better

### Install Steps
#### 1. Get pak0.pk3
First you have to get the **pak0.pk3** file which contains the game data. You can get it for free from **Quake3 Demo** version. Here are some alternatives:
- [download](https://github.com/patdohere/dockerfiles/raw/master/quakejs-stack/quakejs/base/baseq3/pak0.pk3) Quake3 Demo pak0.pk3 directly (**EASY**)
- [step by step](https://slackalaxy.wordpress.com/2015/07/05/quake-iii-demo/) instructions to extract pak0.pk3 from Quake 3 Demo, first [download](http://www.filewatcher.com/m/linuxq3ademo-1.11-6.x86.gz.sh.49289300-0.html
) linuxq3ademo-1.11-6.x86.gz.sh

If you own the full version of the game you can use the **pak0.pk3** from it also.

Then copy **pak0.pk3** to the directory where you execute the application from. E.g. ```./lambdacube-quake3```

#### 2. Compile & Run

You can compile the project using either Stack or Cabal.

##### Stack
```
stack setup
stack build
stack exec q3mapviewer
```

##### Cabal (local build)
```
cabal new-build
./dist-newstyle/build/lambdacube-quake3-0.1.0.0/build/q3mapviewer
```

##### Cabal (global install)
```
cabal install
q3mapviewer
```

### Example Run
When you run it you have to select a map by typing it's name. E.g.
```
bash-3.2$ stack exec q3mapviewer
Available maps:
q3dm1 q3dm17 q3dm7 q3tourney2
Enter map name:
q3tourney2
```
The application will search **pak0.pk3** in the current directory and also will create the ```.lc_q3.cache``` folder to cache the compiled graphics pipelines. It reduces the loading time if the map was loaded before. 
![Quake III level viewer](https://raw.githubusercontent.com/csabahruska/quake3/master/lambdacube-dsl-quake3.jpg)
