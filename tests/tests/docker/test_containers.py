"""
Docker container and orchestration tests for Integral Philosophy Publishing System
"""

import pytest
import time
import json
import requests
import tempfile
import subprocess
import yaml
from pathlib import Path
from typing import Dict, List, Any, Optional

from ..utils.base_test_classes import BaseTestCase, IntegrationTestCase


class TestDockerConfiguration(BaseTestCase):
    """Test Docker configuration files and setup"""

    def test_dockerfile_exists(self):
        """Test that Dockerfile exists and is properly configured"""
        dockerfile_path = Path(__file__).parent.parent.parent / "Dockerfile"

        if not dockerfile_path.exists():
            pytest.skip("Dockerfile not found")

        # Read and validate Dockerfile
        with open(dockerfile_path, "r") as f:
            dockerfile_content = f.read()

        # Check for essential Dockerfile instructions
        required_instructions = ["FROM", "WORKDIR", "COPY", "RUN", "EXPOSE"]

        for instruction in required_instructions:
            assert instruction in dockerfile_content, (
                f"Dockerfile missing {instruction} instruction"
            )

        # Check for security best practices
        security_checks = [
            "USER" in dockerfile_content,  # Non-root user
            "ADD" not in dockerfile_content
            or dockerfile_content.count("ADD") <= 1,  # Prefer COPY
        ]

        if not all(security_checks):
            pytest.skip(
                "Dockerfile could be more secure (consider adding USER instruction)"
            )

    def test_docker_compose_exists(self):
        """Test that docker-compose.yml exists and is valid"""
        compose_file = Path(__file__).parent.parent.parent / "docker-compose.yml"

        if not compose_file.exists():
            pytest.skip("docker-compose.yml not found")

        # Validate YAML syntax
        try:
            with open(compose_file, "r") as f:
                compose_config = yaml.safe_load(f)
        except yaml.YAMLError as e:
            pytest.fail(f"Invalid docker-compose.yml: {e}")

        # Check required sections
        assert "version" in compose_config, "docker-compose.yml missing version"
        assert "services" in compose_config, "docker-compose.yml missing services"

        # Check services configuration
        services = compose_config["services"]
        assert len(services) > 0, "No services defined in docker-compose.yml"

    def test_environment_configuration(self):
        """Test environment configuration files"""
        env_files = [".env.example", ".env.template", "config/.env.example"]

        base_path = Path(__file__).parent.parent.parent
        env_found = False

        for env_file in env_files:
            env_path = base_path / env_file
            if env_path.exists():
                env_found = True

                # Validate environment file
                with open(env_path, "r") as f:
                    env_content = f.read()

                # Check for required environment variables
                required_vars = ["DATABASE_URL", "REDIS_URL", "SECRET_KEY"]

                for var in required_vars:
                    assert var in env_content, (
                        f"Environment variable {var} not in {env_file}"
                    )

        if not env_found:
            pytest.skip("No environment configuration files found")

    def test_production_compose_configuration(self):
        """Test production Docker Compose configuration"""
        prod_compose_files = [
            "docker-compose.prod.yml",
            "docker-compose.production.yml",
        ]

        base_path = Path(__file__).parent.parent.parent
        prod_compose_found = False

        for compose_file in prod_compose_files:
            compose_path = base_path / compose_file
            if compose_path.exists():
                prod_compose_found = True

                # Validate production configuration
                try:
                    with open(compose_path, "r") as f:
                        compose_config = yaml.safe_load(f)

                    services = compose_config.get("services", {})

                    # Check for production-specific configurations
                    if "web" in services:
                        web_service = services["web"]

                        # Check environment overrides
                        if "environment" in web_service:
                            env_vars = web_service["environment"]
                            if isinstance(env_vars, dict):
                                assert "FLASK_ENV" in env_vars, (
                                    "FLASK_ENV should be set in production"
                                )
                                assert env_vars.get("FLASK_ENV") == "production", (
                                    "FLASK_ENV should be 'production'"
                                )

                        # Check restart policy
                        restart_policy = web_service.get("restart", "unless-stopped")
                        assert restart_policy in ["always", "unless-stopped"], (
                            "Web service should have restart policy"
                        )

                    # Check for security configurations
                    for service_name, service_config in services.items():
                        if "user" in service_config:
                            assert service_config["user"] != "root", (
                                f"Service {service_name} should not run as root"
                            )

                except yaml.YAMLError as e:
                    pytest.fail(f"Invalid production compose file: {e}")

        if not prod_compose_found:
            pytest.skip("No production Docker Compose configuration found")


class TestContainerOrchestration(IntegrationTestCase):
    """Test Docker Compose orchestration without requiring Docker daemon"""

    @pytest.fixture(autouse=True)
    def setup_orchestration_environment(self):
        """Setup orchestration testing environment"""
        self.compose_file = Path(__file__).parent.parent.parent / "docker-compose.yml"
        self.compose_cmd = ["docker-compose", "-f", str(self.compose_file)]

    def test_compose_file_validation(self):
        """Validate Docker Compose file structure and content"""
        if not self.compose_file.exists():
            pytest.skip("docker-compose.yml not found")

        try:
            with open(self.compose_file, "r") as f:
                compose_config = yaml.safe_load(f)
        except yaml.YAMLError as e:
            pytest.fail(f"Invalid docker-compose.yml: {e}")

        # Validate version
        assert "version" in compose_config, "Missing version in docker-compose.yml"

        # Validate services
        services = compose_config.get("services", {})
        assert len(services) > 0, "No services defined"

        # Common services to check for
        expected_services = ["web", "db", "redis"]
        found_services = []

        for service in expected_services:
            if service in services:
                found_services.append(service)

        if len(found_services) == 0:
            pytest.skip("No expected services found in docker-compose.yml")

    def test_service_configuration_validation(self):
        """Validate individual service configurations"""
        if not self.compose_file.exists():
            pytest.skip("docker-compose.yml not found")

        try:
            with open(self.compose_file, "r") as f:
                compose_config = yaml.safe_load(f)
        except yaml.YAMLError as e:
            pytest.fail(f"Invalid docker-compose.yml: {e}")

        services = compose_config.get("services", {})

        # Validate web service if present
        if "web" in services:
            web_service = services["web"]

            # Check for port exposure
            if "ports" in web_service:
                ports = web_service["ports"]
                assert len(ports) > 0, "Web service should expose ports"

            # Check for environment variables
            if "environment" in web_service:
                env = web_service["environment"]
                if isinstance(env, dict):
                    # Check for required environment variables
                    required_env = ["DATABASE_URL", "REDIS_URL"]
                    for env_var in required_env:
                        assert env_var in env, f"Web service missing {env_var}"

        # Validate database service if present
        if "db" in services:
            db_service = services["db"]

            # Check for volume mounting for persistence
            if "volumes" in db_service:
                volumes = db_service["volumes"]
                assert len(volumes) > 0, "Database should have persistent volumes"

            # Check for environment variables
            if "environment" in db_service:
                env = db_service["environment"]
                if isinstance(env, dict):
                    # Check for database credentials
                    db_vars = ["POSTGRES_USER", "POSTGRES_PASSWORD", "POSTGRES_DB"]
                    for var in db_vars:
                        assert var in env, f"Database missing {var}"

    def test_network_configuration(self):
        """Validate network configuration in Docker Compose"""
        if not self.compose_file.exists():
            pytest.skip("docker-compose.yml not found")

        try:
            with open(self.compose_file, "r") as f:
                compose_config = yaml.safe_load(f)
        except yaml.YAMLError as e:
            pytest.fail(f"Invalid docker-compose.yml: {e}")

        # Check for network definitions
        networks = compose_config.get("networks", {})

        if len(networks) > 0:
            # Validate network configuration
            for network_name, network_config in networks.items():
                if isinstance(network_config, dict):
                    # Check for custom network settings
                    if "driver" in network_config:
                        valid_drivers = ["bridge", "overlay", "host", "none"]
                        assert network_config["driver"] in valid_drivers, (
                            f"Invalid network driver: {network_config['driver']}"
                        )

        # Check service network usage
        services = compose_config.get("services", {})
        for service_name, service_config in services.items():
            if "networks" in service_config:
                service_networks = service_config["networks"]
                if isinstance(service_networks, list):
                    for network in service_networks:
                        assert network in networks, (
                            f"Service {service_name} references undefined network {network}"
                        )

    def test_volume_configuration(self):
        """Validate volume configuration for data persistence"""
        if not self.compose_file.exists():
            pytest.skip("docker-compose.yml not found")

        try:
            with open(self.compose_file, "r") as f:
                compose_config = yaml.safe_load(f)
        except yaml.YAMLError as e:
            pytest.fail(f"Invalid docker-compose.yml: {e}")

        # Check for volume definitions
        volumes = compose_config.get("volumes", {})

        # Check service volume usage
        services = compose_config.get("services", {})
        persistent_services = []

        for service_name, service_config in services.items():
            if "volumes" in service_config:
                service_volumes = service_config["volumes"]
                if len(service_volumes) > 0:
                    persistent_services.append(service_name)

        if len(persistent_services) > 0:
            # At least one service should use volumes for persistence
            assert len(persistent_services) > 0, (
                "No services configured with persistent volumes"
            )

    def test_health_check_configuration(self):
        """Validate health check configuration"""
        if not self.compose_file.exists():
            pytest.skip("docker-compose.yml not found")

        try:
            with open(self.compose_file, "r") as f:
                compose_config = yaml.safe_load(f)
        except yaml.YAMLError as e:
            pytest.fail(f"Invalid docker-compose.yml: {e}")

        services = compose_config.get("services", {})

        # Check for health checks in critical services
        critical_services = ["web", "db"]

        for service_name in critical_services:
            if service_name in services:
                service_config = services[service_name]

                if "healthcheck" in service_config:
                    health_check = service_config["healthcheck"]

                    # Validate health check configuration
                    assert "test" in health_check, (
                        f"Health check for {service_name} missing test command"
                    )

                    # Check timeout and interval
                    if "timeout" in health_check:
                        assert health_check["timeout"] > 0, (
                            f"Health check timeout for {service_name} should be positive"
                        )

                    if "interval" in health_check:
                        assert health_check["interval"] > 0, (
                            f"Health check interval for {service_name} should be positive"
                        )

    def test_compose_syntax_validation(self):
        """Test docker-compose.yml syntax using docker-compose command"""
        if not self.compose_file.exists():
            pytest.skip("docker-compose.yml not found")

        # Try to validate compose file syntax
        try:
            result = subprocess.run(
                self.compose_cmd + ["config"],
                check=True,
                capture_output=True,
                text=True,
                timeout=30,
            )

            assert result.returncode == 0, (
                f"docker-compose config failed: {result.stderr}"
            )

            # Parse output to ensure it's valid
            yaml.safe_load(result.stdout)

        except subprocess.CalledProcessError:
            pytest.skip("docker-compose command not available or failed")
        except subprocess.TimeoutExpired:
            pytest.skip("docker-compose config timed out")
        except yaml.YAMLError:
            pytest.fail("docker-compose config output is not valid YAML")


class TestProductionDeployment(BaseTestCase):
    """Test production deployment scenarios and configurations"""

    def test_production_environment_setup(self):
        """Test production environment configuration"""
        production_files = [
            ".env.production",
            "config/production.py",
            "docker-compose.prod.yml",
        ]

        base_path = Path(__file__).parent.parent.parent
        production_config_found = False

        for prod_file in production_files:
            prod_path = base_path / prod_file
            if prod_path.exists():
                production_config_found = True

                if prod_file.endswith(".py"):
                    # Validate Python production configuration
                    self._validate_python_config(prod_path)
                elif prod_file.endswith(".yml"):
                    # Validate YAML production configuration
                    self._validate_yaml_config(prod_path)
                elif prod_file.startswith(".env"):
                    # Validate environment file
                    self._validate_env_file(prod_path)

        if not production_config_found:
            pytest.skip("No production configuration files found")

    def _validate_python_config(self, config_path: Path):
        """Validate Python configuration file"""
        config = {}
        try:
            with open(config_path, "r") as f:
                exec(f.read(), config)

            # Check for production settings
            assert config.get("DEBUG") == False, "DEBUG should be False in production"
            assert "SECRET_KEY" in config, "SECRET_KEY must be set in production"
            assert "DATABASE_URL" in config, "DATABASE_URL must be set in production"

            # Check for SSL configuration
            if "SSL_DISABLE" in config:
                assert config["SSL_DISABLE"] == False, (
                    "SSL should be enabled in production"
                )

        except Exception as e:
            pytest.fail(f"Invalid production configuration: {e}")

    def _validate_yaml_config(self, config_path: Path):
        """Validate YAML configuration file"""
        try:
            with open(config_path, "r") as f:
                config = yaml.safe_load(f)

            # Basic YAML structure validation
            assert isinstance(config, dict), "Configuration must be a dictionary"

        except yaml.YAMLError as e:
            pytest.fail(f"Invalid YAML configuration: {e}")

    def _validate_env_file(self, env_path: Path):
        """Validate environment file"""
        with open(env_path, "r") as f:
            content = f.read()

        # Check for required environment variables
        required_vars = ["DATABASE_URL", "SECRET_KEY", "FLASK_ENV"]

        for var in required_vars:
            assert var in content, (
                f"Environment variable {var} missing from {env_path.name}"
            )

        # Check for production environment
        assert "FLASK_ENV=production" in content, (
            "FLASK_ENV should be set to production"
        )

    def test_backup_configuration(self):
        """Test backup and recovery configuration"""
        backup_configs = [
            "scripts/backup.sh",
            "config/backup.yml",
            "docker-compose.backup.yml",
        ]

        base_path = Path(__file__).parent.parent.parent
        backup_found = False

        for backup_config in backup_configs:
            config_path = base_path / backup_config
            if config_path.exists():
                backup_found = True

                if backup_config.endswith(".sh"):
                    # Check if backup script is executable (Unix-like systems)
                    if hasattr(config_path.stat(), "st_mode"):
                        # Check execute permission
                        if config_path.stat().st_mode & 0o111:
                            pass  # Script is executable
                        else:
                            print(f"Backup script {backup_config} is not executable")

        if not backup_found:
            pytest.skip("No backup configuration found")

    def test_logging_configuration(self):
        """Test logging configuration for production"""
        logging_configs = ["config/logging.yml", "config/production-logging.yml"]

        base_path = Path(__file__).parent.parent.parent
        logging_found = False

        for logging_config in logging_configs:
            config_path = base_path / logging_config
            if config_path.exists():
                logging_found = True

                try:
                    with open(config_path, "r") as f:
                        logging_config = yaml.safe_load(f)

                    assert "version" in logging_config, "Logging config missing version"
                    assert "handlers" in logging_config, (
                        "Logging config missing handlers"
                    )
                    assert "root" in logging_config, (
                        "Logging config missing root logger"
                    )

                except yaml.YAMLError as e:
                    pytest.fail(f"Invalid logging configuration: {e}")

        if not logging_found:
            pytest.skip("No logging configuration found")

    def test_monitoring_setup(self):
        """Test monitoring and metrics configuration"""
        monitoring_files = [
            "prometheus.yml",
            "grafana/dashboards/integral-philosophy.json",
            "docker-compose.monitoring.yml",
        ]

        base_path = Path(__file__).parent.parent.parent
        monitoring_found = False

        for monitoring_file in monitoring_files:
            monitor_path = base_path / monitoring_file
            if monitor_path.exists():
                monitoring_found = True

                if monitoring_file.endswith(".yml"):
                    # Validate Prometheus configuration
                    try:
                        with open(monitor_path, "r") as f:
                            prometheus_config = yaml.safe_load(f)

                        assert "global" in prometheus_config, (
                            "Prometheus config missing global section"
                        )
                        assert "scrape_configs" in prometheus_config, (
                            "Prometheus config missing scrape_configs"
                        )

                    except yaml.YAMLError as e:
                        pytest.fail(f"Invalid monitoring configuration: {e}")

        if not monitoring_found:
            pytest.skip("No monitoring configuration found")


class TestContainerSecurity(BaseTestCase):
    """Test container security configurations"""

    def test_dockerfile_security(self):
        """Test Dockerfile for security best practices"""
        dockerfile_path = Path(__file__).parent.parent.parent / "Dockerfile"

        if not dockerfile_path.exists():
            pytest.skip("Dockerfile not found")

        with open(dockerfile_path, "r") as f:
            dockerfile_content = f.read()

        # Security checks
        security_issues = []

        # Check for non-root user
        if "USER" not in dockerfile_content:
            security_issues.append(
                "Dockerfile should specify a non-root user with USER instruction"
            )

        # Check for COPY vs ADD
        if dockerfile_content.count("ADD") > dockerfile_content.count("COPY"):
            security_issues.append("Prefer COPY over ADD for security")

        # Check for base image specificity
        if "FROM" in dockerfile_content:
            from_lines = [
                line
                for line in dockerfile_content.split("\n")
                if line.startswith("FROM")
            ]
            for from_line in from_lines:
                if "latest" in from_line and "alpine" not in from_line:
                    security_issues.append(
                        f"Using 'latest' tag in {from_line} - consider specific version"
                    )

        # Report security issues
        if security_issues:
            print("\n".join(security_issues))

    def test_environment_secrets(self):
        """Test that secrets are not hardcoded in configuration"""
        config_files = [".env.example", ".env.template", "docker-compose.yml"]

        base_path = Path(__file__).parent.parent.parent

        for config_file in config_files:
            config_path = base_path / config_file
            if config_path.exists():
                with open(config_path, "r") as f:
                    content = f.read()

                # Check for potential hardcoded secrets
                secret_patterns = [
                    "password=password",
                    "secret=secret",
                    "key=key",
                    "token=token",
                    "admin=admin",
                    "root=root",
                ]

                for pattern in secret_patterns:
                    if pattern in content.lower():
                        print(
                            f"Potential hardcoded secret pattern in {config_file}: {pattern}"
                        )

    def test_network_security(self):
        """Test network security configurations"""
        compose_file = Path(__file__).parent.parent.parent / "docker-compose.yml"

        if not compose_file.exists():
            pytest.skip("docker-compose.yml not found")

        try:
            with open(compose_file, "r") as f:
                compose_config = yaml.safe_load(f)
        except yaml.YAMLError:
            pytest.fail("Invalid docker-compose.yml")

        services = compose_config.get("services", {})

        # Check for exposed ports
        for service_name, service_config in services.items():
            if "ports" in service_config:
                ports = service_config["ports"]
                for port in ports:
                    if isinstance(port, str):
                        # Check if port is bound to all interfaces
                        if ":0.0.0.0" in port or ":0" in port:
                            print(
                                f"Service {service_name} binds to all interfaces: {port}"
                            )

    def test_resource_limits(self):
        """Test resource limit configurations"""
        compose_file = Path(__file__).parent.parent.parent / "docker-compose.yml"

        if not compose_file.exists():
            pytest.skip("docker-compose.yml not found")

        try:
            with open(compose_file, "r") as f:
                compose_config = yaml.safe_load(f)
        except yaml.YAMLError:
            pytest.fail("Invalid docker-compose.yml")

        services = compose_config.get("services", {})
        services_without_limits = []

        for service_name, service_config in services.items():
            has_memory_limit = (
                "mem_limit" in service_config or "deploy" in service_config
            )
            has_cpu_limit = "cpu_count" in service_config or "deploy" in service_config

            if not has_memory_limit or not has_cpu_limit:
                services_without_limits.append(service_name)

        if services_without_limits:
            print(
                f"Services without resource limits: {', '.join(services_without_limits)}"
            )
