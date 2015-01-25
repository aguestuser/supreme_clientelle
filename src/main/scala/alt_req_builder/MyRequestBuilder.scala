package alt_req_builder

import java.util.ArrayList

import com.ning.http.client.{FluentCaseInsensitiveStringsMap, Request, RequestBuilder}
import java.io.{File, InputStream}
import java.lang.StringBuilder
import java.net.{InetAddress, MalformedURLException, URI}
import java.util.Collections

import com.ning.http.client.{BodyGenerator, ConnectionPoolKeyStrategy, DefaultConnectionPoolStrategy, FluentStringsMap, Part, PerRequestConfig, ProxyServer, Realm}
import com.ning.http.client.Request.EntityWriter
import com.ning.http.client.cookie.Cookie
import com.ning.http.util.UTF8UrlEncoder

/**
 * Created by aguestuser on 1/24/15.
 *
 * borrowed in full from Brandon Hudgeon (@bhgudgeon)
 * see: https://gist.github.com/bhudgeons/5888582
 *
 * Scala port of com.ning.http.client.RequestBuilder
 * to override overcome the standard RequestBuilder's
 * attempts to "fix" non-standard URL query strings.
 *
 * For motivation and walkthrough, see:
 * https://bhudgeons.telegr.am/blog_posts/handling-non-standard-urls-in-dispatch
 */

class MyRequestBuilder extends RequestBuilder {

  class MyRequestImpl extends Request {

    var method: String = _
    var url = ""
    var headers = new FluentCaseInsensitiveStringsMap();
    var cookies = new ArrayList[Cookie]();
    var byteData: Array[Byte] = _
    var stringData: String = _
    var streamData: InputStream = _
    var entityWriter: EntityWriter = _
    var bodyGenerator: BodyGenerator = _
    var params: FluentStringsMap = _
    var parts: ArrayList[Part] = _
    var virtualHost: String = _
    var length: Long = -1;
    var queryParams: FluentStringsMap = _
    var proxyServer: ProxyServer = _
    var realm: Realm = _
    var file: File = _
    var followRedirects: Boolean = _
    var perRequestConfig: PerRequestConfig = _
    var rangeOffset: Long = 0;
    var connectionPoolKeyStrategy: ConnectionPoolKeyStrategy = DefaultConnectionPoolStrategy.INSTANCE;
    var address: InetAddress = _
    var localAddress: InetAddress = _
    var charset: String = _
    var useRawUrl: Boolean = _
    var rawUri: java.net.URI = _
    var originalUri: java.net.URI = _
    var uri: java.net.URI = _

    def bodyGenerator(x$1: com.ning.http.client.BodyGenerator): Unit = this.bodyGenerator = x$1
    def byteData(x$1: Array[Byte]): Unit = this.byteData = x$1
    def entityWriter(x$1: com.ning.http.client.Request.EntityWriter): Unit = this.entityWriter = x$1
    def file(x$1: java.io.File): Unit = this.file = x$1
    def followRedirects(x$1: Boolean): Unit = this.followRedirects = x$1
    def method(x$1: String): Unit = this.method = x$1
    def params(x$1: com.ning.http.client.FluentStringsMap): Unit = this.params = x$1
    def parts(x$1: java.util.ArrayList[com.ning.http.client.Part]): Unit = this.parts = x$1
    def perRequestConfig(x$1: com.ning.http.client.PerRequestConfig): Unit = this.perRequestConfig = x$1
    def proxyServer(x$1: com.ning.http.client.ProxyServer): Unit = this.proxyServer = x$1
    def queryParams(x$1: com.ning.http.client.FluentStringsMap): Unit = this.queryParams = x$1
    def realm(x$1: com.ning.http.client.Realm): Unit = this.realm = x$1
    def streamData(x$1: java.io.InputStream): Unit = this.streamData = x$1
    def stringData(x$1: String): Unit = this.stringData = x$1
    def virtualHost(x$1: String): Unit = this.virtualHost = x$1
    def getBodyEncoding(): String = charset
    def getConnectionPoolKeyStrategy(): com.ning.http.client.ConnectionPoolKeyStrategy = connectionPoolKeyStrategy
    def getContentLength(): Long = length
    def getInetAddress(): java.net.InetAddress = address
    def getLocalAddress(): java.net.InetAddress = localAddress
    def isRedirectOverrideSet(): Boolean = followRedirects != null
    def isUseRawUrl(): Boolean = useRawUrl
    def setUrl(url: String) = this.url = url
    def getRawURI() = uri
    def getOriginalURI() = uri
    def getURI() = uri

    def MyRequestImpl(prototype: Request) = {
      if (prototype != null) {
        this.method = prototype.getMethod();
        val pos = prototype.getUrl().indexOf("?");
        this.url = if (pos > 0) prototype.getUrl().substring(0, pos) else prototype.getUrl();
        this.headers = new FluentCaseInsensitiveStringsMap(prototype.getHeaders());
        this.cookies = new ArrayList[Cookie](prototype.getCookies());
        this.byteData = prototype.getByteData();
        this.stringData = prototype.getStringData();
        this.streamData = prototype.getStreamData();
        this.entityWriter = prototype.getEntityWriter();
        this.bodyGenerator = prototype.getBodyGenerator();
        this.params = if (prototype.getParams() == null) null else new FluentStringsMap(prototype.getParams());
        this.queryParams = if (prototype.getQueryParams() == null) null else new FluentStringsMap(prototype.getQueryParams());
        this.parts = if (prototype.getParts() == null) null else new ArrayList[Part](prototype.getParts());
        this.virtualHost = prototype.getVirtualHost();
        this.length = prototype.getLength();
        this.proxyServer = prototype.getProxyServer();
        this.realm = prototype.getRealm();
        this.file = prototype.getFile();
        this.followRedirects = prototype.isRedirectEnabled();
        this.perRequestConfig = prototype.getPerRequestConfig();
        this.rangeOffset = prototype.getRangeOffset();
        this.connectionPoolKeyStrategy = prototype.getConnectionPoolKeyStrategy();
        this.address = prototype.getInetAddress();
        this.localAddress = prototype.getLocalAddress();
        this.charset = prototype.getBodyEncoding();
        this.useRawUrl = prototype.isUseRawUrl();
      }
    }

    def getReqType() = { getMethod(); }
    def getMethod() = { method }
    def getUrl(): String = { toUrl(true); }

    def toUrl(encode: Boolean): String = {

      if (url == null) { url = "http://localhost";}
      var uri: String = ""
      try {
        uri = URI.create(url).toURL().toString();
      } catch {
        case e: MalformedURLException => throw new IllegalStateException("Illegal URL: " + url, e);
      }
      if (queryParams != null) {
        val builder = new StringBuilder();
        if (!url.substring(8).contains("/")) {
          builder.append("/");
        }
        builder.append("?");
        val i = queryParams.iterator()
        while (i.hasNext()) {
          //Map.Entry<String, List<String>>
          val param = i.next();
          val name = param.getKey();
          val j = param.getValue().iterator()
          while (j.hasNext()) {
            val value = j.next();
            if (encode) {
              UTF8UrlEncoder.appendEncoded(builder, name);
            } else {
              builder.append(name);
            }
            if (value != null && !value.equals("")) {
              builder.append('=');
              if (encode) {
                UTF8UrlEncoder.appendEncoded(builder, value);
              } else {
                builder.append(value);
              }
            }
            if (j.hasNext()) {
              builder.append('&');
            }
          }
          if (i.hasNext()) {
            builder.append('&');
          }
        }
        uri += builder.toString();
      }
      uri;
    }
    def getRawUrl(): String = { toUrl(false); }
    def getHeaders() = { headers; }
    def getCookies() = { Collections.unmodifiableCollection(cookies); }
    def getByteData() = { byteData; }
    def getStringData() = { stringData; }
    def getStreamData() = { streamData; }
    def getEntityWriter() = { entityWriter; }
    def getBodyGenerator() = { bodyGenerator; }
    def getLength() = { length; }
    def getParams() = { params; }
    def getParts() = { parts; }
    def getVirtualHost() = { virtualHost; }
    def getQueryParams() = { queryParams; }
    def getProxyServer() = { proxyServer; }
    def getRealm() = { realm; }
    def getFile() = { file; }
    def isRedirectEnabled() = { followRedirects; }
    def getPerRequestConfig() = { perRequestConfig; }
    def getRangeOffset() = { rangeOffset; }

    @Override
    override def toString = {
      val sb = new StringBuilder(url);
      sb.append("\t");
      sb.append(method);
      import scala.collection.JavaConversions._
      val heads = headers.keySet().toList
      for (name <- heads) {
        sb.append("\t");
        sb.append(name);
        sb.append(":");
        sb.append(headers.getJoinedValue(name, ", "));
      }
      sb.toString();
    }
  }

  var myrequest: MyRequestImpl = new MyRequestImpl()
  def setUrl[T](url: String) = {
    myrequest.asInstanceOf[MyRequestImpl].setUrl(url) // just set the URL like we passed it in. Skip buildUrl!
    myrequest.uri = new URI(url)
    this
  }

  override def setMethod(method: String) = {
    myrequest.method = method;
    this
  }

  override def build(): Request = {
    if ((myrequest.length < 0) && (myrequest.streamData == null) &&
      (("POST".equals(myrequest.method)) || ("PUT".equals(myrequest.method)))) {
      // can't concatenate content-length
      val contentLength = myrequest.headers.getFirstValue("Content-Length");
      if (contentLength != null) {
        try {
          myrequest.length = java.lang.Long.parseLong(contentLength);
        } catch {
          case e: NumberFormatException => // NoOp -- we wdn't specify length so it will be chunked?
        }
      }
    }
    myrequest
  }
}