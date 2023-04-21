# ABAP 2 JSON
<!-- markdown-link-check-disable-next-line -->
[![REUSE status](https://api.reuse.software/badge/github.com/SAP/abap-file-formats)](https://api.reuse.software/info/github.com/SAP/abap-file-formats)

## About this project

This is Open source version of standard /UI2/CL_JSON class and its public documentation were previosly available on SCN wiki as "One more ABAP to JSON Serializer and Deserializer" from 2013. 

### Why
There are a lot of other implementations of the **ABAP to JSON Serializer and Deserializer** in SDN, but for different reasons, all implementations I have found were not suitable for my needs. From SAP_BASIS 7.40 there is also **a simple transformation** available for converting **ABAP to JSON** and **JSON to ABAP**. It is the best choice if you need maximal performance and do not care about **serialization format**, but for proper handling of ABAP types and name **pretty-printing**, it fits badly. 

So, I have written my **ABAP JSON serializer and ABAP JSON deserializer** which has some key differences from other implementations.

Below you can find a snippet of the ABAP JSON class I wrote, that you can use as a local class or as a global after renaming.

An original and actual version of the source can be found in class /UI2/CL_JSON delivered with UI2 Add-on (can be applied to SAP_BASIS 700 – 76X). So, you can use this ABAP JSON parser in your standard code mostly on any system.

## Requirements and Setup

Install via [abapGit Eclipse plugin](https://github.com/abapGit/ADT_Frontend) on ABAP cloud systems and [abapGit for SAPGUI](https://docs.abapgit.org/guide-online-install.html) on systems with SAP_BASIS 7.57 or higher.

## Support, Feedback, Contributing

This project is open to feature requests/suggestions, bug reports etc. via [GitHub issues](https://github.com/SAP/abap-to-json/issues). Contribution and feedback are encouraged and always welcome. For more information about how to contribute, the project structure, as well as additional contribution information, see our [Contribution Guidelines](CONTRIBUTING.md).

## Code of Conduct

We as members, contributors, and leaders pledge to make participation in our community a harassment-free experience for everyone. By participating in this project, you agree to abide by its [Code of Conduct](https://github.com/SAP/.github/blob/main/CODE_OF_CONDUCT.md) at all times.

## Licensing

Copyright 2013-2023 SAP SE or an SAP affiliate company and <your-project> contributors. Please see our [LICENSE](LICENSE) for copyright and license information. Detailed information including third-party components and their licensing/copyright information is available [via the REUSE tool](https://api.reuse.software/info/github.com/SAP/<your-project>).
