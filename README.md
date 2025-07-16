# ABAP to JSON serializer and deserializer
<!-- markdown-link-check-disable-next-line -->
[![REUSE status](https://api.reuse.software/badge/github.com/SAP/abap-to-json)](https://api.reuse.software/info/github.com/SAP/abap-to-json)

## About this project

This is an Open-source version of the standard /UI2/CL_JSON class, and its public documentation was previously available on the SCN wiki as "One more ABAP to JSON Serializer and Deserializer" in 2013.
The official delivery of /UI2/CL_JSON will continue. The Open Source version (Z_UI2_JSON) is your way to contribute and get updates sooner using ABAP Git infrastructure. 

### Why
There are a lot of other implementations of the **ABAP to JSON Serializer and Deserializer** in SDN. Still, I found all implementations unsuitable for my needs for different reasons. From SAP_BASIS 7.40, there is also **a simple transformation** available for converting **ABAP to JSON** and **JSON to ABAP**. It is the best choice if you need maximal performance and do not care about **serialization format**, but it fits badly for properly handling ABAP types and name **pretty-printing**. 

So, I have written my **ABAP JSON serializer** and **ABAP JSON deserializer**, which have some key differences from other implementations.

Here, you can find an Open-Source version of the standard /UI2/CL_JSON class in the form of a Z* class that you can use as a local or global one.

An original and current source version can be found in class /UI2/CL_JSON delivered with UI2 Add-on (can be applied to SAP_BASIS 740 – 76X). So, you can use this ABAP JSON parser in your standard code, mostly on any system.

The minimum required SAP_BASIS version for the class is SAP_BASIS 7.31 (ABAP language dependency, e.g., usage of "escape" function).

## Alternatives
If, for some reason, the solution does not fit your purposes, there are other alternatives you may try:
* [zJSON from Uwe Fetzer (aka se38)](https://github.com/se38/zJSON)
* [aJSON from Alexander Tsybulsky (aka sbcgua)](https://github.com/sbcgua/ajson)
* [ABAPify JSON ( ZCL_JSON )](https://github.com/abapify/json)

## Documentation
* [Base Usage](docs/basic.md)
* [Advanced Usage](docs/advanced.md)
* [Class Extension](docs/class-extension.md)
* [Dynamic Data Accessor](docs/data-access.md)
* [FAQ](docs/faq.md)
* [Version History](docs/history.md)

## The Code
* [Code in abapGit format](src)

## Requirements and Setup

Install via [abapGit Eclipse plugin](https://github.com/abapGit/ADT_Frontend) on ABAP cloud systems and [abapGit for SAPGUI](https://docs.abapgit.org/guide-online-install.html) on systems with SAP_BASIS 7.57 or higher.

## Support, Feedback, Contributing

This project is open to feature requests/suggestions, bug reports, etc. via [GitHub issues](https://github.com/SAP/abap-to-json/issues). Contribution and feedback are encouraged and always welcome. For more information about how to contribute, the project structure, and additional contribution information, see our [Contribution Guidelines](CONTRIBUTING.md). The contribution in the Open Source version will then be integrated into the standard SAP-delivered version (/UI2/CL_JSON). 
You can also use [Github Discussion](https://github.com/SAP/abap-to-json/discussions) to comment, request features, or discuss the behavior.

## Code of Conduct

Members, contributors, and leaders pledge to make participation in our community a harassment-free experience. By participating in this project, you agree to always abide by its [Code of Conduct](https://github.com/SAP/.github/blob/main/CODE_OF_CONDUCT.md).

## Licensing

Copyright 2013-2023 SAP SE or an SAP affiliate company and <your-project> contributors. Please see our [LICENSE](LICENSE) for copyright and license information. Detailed information, including third-party components and their licensing/copyright information, is available [via the REUSE tool](https://api.reuse.software/info/github.com/SAP/abap-to-json).
