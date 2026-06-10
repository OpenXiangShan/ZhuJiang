# OpenZhuJiang

OpenZhuJiang is a CHI-based interconnect and cache-integration repository used in the OpenXiangShan ecosystem. It contains the ZhuJiang top-level system, XiJiang network components, DongJiang data-path and directory components, and the supporting test tops used to elaborate and generate RTL.

## Directory structure

Some of the key directories are shown below.

```text
.
├── src
│   ├── main/scala
│   │   ├── dongjiang      # CHI data-path, directory, and supporting components
│   │   ├── xijiang        # network, router, ring, and traffic-simulation logic
│   │   └── zhujiang       # top-level system integration
│   └── test/scala
│       ├── xijiang        # traffic simulation and XiJiang test tops
│       └── zhujiang       # ZhuJiang and SoC-level test tops
├── xs-utils               # shared utility library
├── rocket-chip            # local Rocket Chip dependency
├── build.sc               # Mill build definition
└── Makefile               # convenience targets for build and RTL generation
```

## Quick start

Initialize submodules:

```bash
make init
```

Compile source code and tests:

```bash
make comp
```

Reformat the codebase:

```bash
make reformat
```

Generate the default ZhuJiang top RTL:

```bash
make verilog
```

Generated RTL is written to `build/rtl` by default.
