# StatEdu Studio 1.3.1 맥 앱

정식 공개판 1.3.1을 Apple Silicon 맥으로 변환하는 빌드 구성입니다. 앱 버전은 `1.3.1`, ID는 `com.statedu.studio.mac`입니다.

저장소 루트의 `docs/MACOS_BUILD_KO.md` 순서대로 스테이지를 만들고 고정 런타임을 준비하십시오. R.framework가 준비된 스테이지에서는 `sh build.command`로 로컬 앱을 생성할 수 있습니다. Python 3.10 이상, Node 22.12 이상과 npm, Xcode Command Line Tools가 필요합니다.

앱은 공개판 메뉴를 적용하고 Mplus는 비활성화합니다. 내장 R과 Electron PDF를 사용하므로 외부 R·Chrome 설치가 필요하지 않습니다.

일반 배포용 Developer ID 서명·Apple 공증은 `scripts/build_macos_release.py`로 별도 수행합니다. 인증서와 공증 프로필 없이 만든 로컬 패키지는 정식 1.3.1 기능을 사용하지만 서명·공증 완료 설치본은 아닙니다.
