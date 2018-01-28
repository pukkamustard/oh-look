let

region = "eu-central-1";

in
  {
    the-island = { resources, pkgs, ... }:
      { deployment.targetEnv = "ec2";
        deployment.ec2.region = region;
        deployment.ec2.instanceType = "t2.nano";
        deployment.ec2.keyPair = resources.ec2KeyPairs.my-key-pair;
        deployment.ec2.securityGroups = [ resources.ec2SecurityGroups.my-sg ];

        deployment.route53.hostName = "oh-look.smplfy.ch";

        services.nginx = {
          enable = true;
          recommendedProxySettings = true;

          virtualHosts."oh-look.smplfy.ch" = {
            enableACME = true;

            locations."/" = {
              root = "/home/island/oh-look/docs/";
            };

            locations."/server" = {
              proxyWebsockets = true;
              proxyPass = "http://127.0.0.1:9998";
            };

          };
        };

        networking.firewall = {
          allowedTCPPorts = [ 22 443 80];
        };

        environment.systemPackages = with pkgs; [
          vim
          git
          tmux
          nodejs-8_x
        ];

        users.extraUsers.island = {
          isNormalUser = true;
        };
      };



    # Provision an EC2 key pair.
    resources.ec2KeyPairs.my-key-pair = { 
      inherit region; 
    };

    resources.ec2SecurityGroups.my-sg = {
      description = "Allow SSH access";
      inherit region;
      rules = [
        {
          fromPort = 22;
          toPort = 22;
          sourceIp = "0.0.0.0/0";
        }
        {
          fromPort = 443;
          toPort = 443;
          sourceIp = "0.0.0.0/0";
        }
        {
          fromPort = 80;
          toPort = 80;
          sourceIp = "0.0.0.0/0";
        }
      ];
    };

}
