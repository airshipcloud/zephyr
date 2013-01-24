# Cloud Fabric

Open source cloud framework to make scalable, real-time apps in a fraction of the time.

### Cloud Fabric is a collection of modular components:

1. Scalable data store with a fully customizable security layer
2. Real-time event notification when data changes
3. Automatic networking and syncing with other Cloud Fabric servers


## Philosophy

Cloud Fabric can run as a lightweight personal cloud server to manage a single user's personal data or as a scalable app server framework.

With Cloud Fabric, you can easily create apps in HTML and JavaScript without the need to write server code. However, as your app complexity grows, you can easily add server-side logic in any language.

As a philosophy, Cloud Fabric is less of a traditional framework and more of a set of simple service APIs. The services talk to each other on an internal network and expose one external endpoint for communication with apps and other Cloud Fabric servers. Any service can be exchanged or customized without affect to the other services as long as the APIs remain the same.

The code provided in the repository is meant to serve as a fully functional, production ready server. However, any server or component that conforms to the Cloud Fabric API will be linked to from this document.
