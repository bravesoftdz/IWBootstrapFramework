object IWServerController: TIWServerController
  OldCreateOrder = False
  AppName = 'MyApp'
  Description = 'My IntraWeb Application'
  DisplayName = 'IntraWeb Application'
  Port = 8087
  ServerResizeTimeout = 0
  ShowLoadingAnimation = True
  SessionTimeout = 4
  SSLOptions.NonSSLRequest = nsAccept
  SSLOptions.Port = 0
  SSLOptions.SSLVersions = []
  URLBase = '/iwbootstrap/'
  Version = '14.0.53'
  AllowMultipleSessionsPerUser = False
  JavaScriptOptions.RenderjQuery = False
  HttpKeepAlive = True
  RestartExpiredSession = True
  OnConfig = IWServerControllerBaseConfig
  OnNewSession = IWServerControllerBaseNewSession
  Height = 310
  Width = 342
end
